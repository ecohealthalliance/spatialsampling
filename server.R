################################################################################
#
# Server
#
################################################################################

##
function(input, output, session) {
  ##############################################################################
  #
  # Condition-based UIs
  #
  ##############################################################################

  ## UI for main page
  output$survey_area_input <- renderUI({
    if (input$input_type == "gpkg") {
      fileInput(inputId = "survey_area",
                label = "Upload geopackage file of area map",
                accept = "gpkg"
      )
    } else {
      shinyDirButton(id = "survey_area",
        label = "Upload shapefile folder of area map",
        title = "Select shapefile folder to upload"
      )
    }
  })

  ## Choose a directory for SHP folder for survey area map upload
  shinyDirChoose(input = input, session = session,
    id = "survey_area",
    roots = c("wd" = ".", "home" = "/home")
  )

  ## UI for sampling parameters
  output$sample_parameters1 <- renderUI({
    req(input$survey_area)

    ## Sampling units
    numericInput(inputId = "nSamplingUnits",
                 label = "Number of sampling units",
                 min = 20, max = 600,
                 value = 30, step = 1)
  })

  output$sample_parameters2 <- renderUI({
    req(input$survey_area)

    ## Spatial buffer
    numericInput(inputId = "samplingBuffer",
                 label = "Set an area sampling buffer",
                 min = 0, max = 10,
                 value = 0, step = 1)
  })

  output$sample_parameters3 <- renderUI({
    req(input$survey_area)

    ## Sampling button
    actionButton(inputId = "get_sample",
                 label = "Sample",
                 icon = icon(name = "table", lib = "font-awesome"))
  })

  ## UI for sample report button
  output$report_output <- renderUI({
    req(input$get_sample)

    ## Report button - action
    actionButton(inputId = "make_report",
      label = "Report",
      icon = icon(name = "clipboard", lib = "font-awesome")
    )

    ## Report button - download
    #downloadButton(outputId = "create_report",
    #  label = "Report",
    #  icon = icon(name = "clipboard", lib = "font-awesome"))
  })

  ## UI for sample list button
  output$list_output <- renderUI({
    req(input$get_sample)

    ## list button - download
    downloadButton(outputId = "sample_list",
      label = "List")
  })

  ## UI for dataset input
  output$dataset_input <- renderUI({
    if (input$input_type_alt == "gpkg") {
      if (input$dataset_type == "humans") {
        fileInput(inputId = "dataset_human_alt",
          label = "Upload gpkg file of human population dataset",
          accept = "gpkg"
        )
      } else {
        fileInput(inputId = "dataset_cattle_alt",
          label = "Upload gpkg file of cattle population dataset",
          accept = "gpkg"
        )
      }
    } else {
      if (input$dataset_type == "humans") {
        shinyDirButton(id = "dataset_human_alt",
          label = "Upload shapefile folder",
          title = "Select shp folder of human population dataset"
        )
      } else {
        shinyDirButton(id = "dataset_cattle_alt",
          label = "Upload shapefile folder",
          title = "Select shp folder of cattle population dataset"
        )
      }
    }
  })

  ## Choose a directory for SHP folder for population dataset
  shinyDirChoose(input = input, session = session,
    id = "dataset_human_alt",
    roots = c("wd" = ".", "home" = "/home")
  )

  ## Choose a directory for SHP folder for population dataset
  shinyDirChoose(input = input, session = session,
    id = "dataset_cattle_alt",
    roots = c("wd" = ".", "home" = "/home")
  )

  ## UI for boundaries input
  output$boundaries_input <- renderUI({
    if (input$input_type_boundaries == "gpkg") {
      fileInput(inputId = "boundary_map",
        label = "Upload geopackage file of area map",
        accept = "gpkg"
      )
    } else {
      shinyDirButton(id = "boundary_map",
        label = "Upload shapefile folder of area map",
        title = "Select shapefile folder to upload"
      )
    }
  })

  ## Choose a directory for SHP folder for population dataset
  shinyDirChoose(input = input, session = session,
    id = "boundary_map",
    roots = c("wd" = ".", "home" = "/home")
  )

  ##############################################################################
  #
  # Process inputs
  #
  ##############################################################################

  ## Process file paths
  survey_area_file_path <- reactive({
    req(input$survey_area)
    parseDirPath(roots = c("wd" = ".", "home" = "/home"),
                 selection = input$survey_area)
  })

  ## Process input layers
  survey_area <- reactive({
    if (input$input_type == "gpkg") {
      req(input$survey_area)
      file <- input$survey_area
      readOGR(dsn = file$datapath)
    } else {
      req(survey_area_file_path())
      files <- list.files(survey_area_file_path())
      layer <- files %>%
        stringr::str_split(pattern = "\\.", simplify = TRUE)
      readOGR(dsn = survey_area_file_path(),
              layer = layer[1])
    }
  })

  ## Process administrative boundaries

  ## Get coordinates for selected country
  country_coordinates <- reactive({
    req(input$country != " ")
    mb_geocode(search_text = input$country)
  })

  ## Process file paths
  boundary_map_file_path <- reactive({
    req(input$boundary_map)
    parseDirPath(roots = c("wd" = ".", "home" = "/home"),
                 selection = input$boundary_map)
  })

  ## Get boundary files from GADM for chosen country
  admin_boundaries <- reactive({
    ## Progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(
      message = paste("Retrieving administrative boundaries map of ",
                       input$country, sep = ""),
      value = 0.7
    )

    if (!is.null(input$boundary_map)) {
      if (input$input_type_boundaries == "gpkg") {
        req(input$boundary_map)
        file <- input$boundary_map
        readOGR(dsn = file$datapath)
      } else {
        req(boundary_map_file_path())
        files <- list.files(boundary_map_file_path())
        layer <- files %>%
          stringr::str_split(pattern = "\\.", simplify = TRUE)
        readOGR(dsn = boundary_maxp_file_path(),
                layer = layer[1])
      }
    } else {
      ## Read data from GADM
      country_code <- countrycode(input$country,
                                  origin = "country.name",
                                  destination = "iso3c")

      raster::getData(country = country_code,
                      level = 3,
                      path = "www/maps")
    }
  })

  ## Create spatial sample - points
  sampling_points <- eventReactive(input$get_sample, {
    req(input$nSamplingUnits)

    create_sp_grid(x = survey_area(), country = input$country,
                   n = input$nSamplingUnits, buffer = input$samplingBuffer,
                   type = "csas")
  })

  ## Create spatial sample - grid
  sampling_grid <- reactive({
    req(sampling_points())

    sampling_points() %>%
      SpatialPixels() %>%
      as("SpatialPolygons")
  })

  ## Get grid populations
  sampling_pops <- reactive({
    req(sampling_grid(), dataset_worldpop(), dataset_glw())

    ## Show progress bar
    progress <- Progress$new(min = 1, max = length(sampling_grid()))
    on.exit(progress$close())
    progress$set(
      message = paste("Getting population data for each sampling grid"))

    ## Get human and cattle pop
    pops <- data.frame(matrix(nrow = length(sampling_grid()), ncol = 2))
    names(pops) <- c("human_pop", "cattle_pop")

    for (i in seq_len(length(sampling_grid()))) {
      progress$set(value = i)
      subGridHuman <- raster::intersect(dataset_worldpop(), sampling_grid()[i])
      subGridCattle <- raster::intersect(dataset_glw(), sampling_grid()[i])
      pops[i, ] <- c(sum(values(subGridHuman), na.rm = TRUE),
                     sum(values(subGridCattle), na.rm = TRUE))
    }

    pops
  })

  ## Get sampling points information
  sampling_points_info <- reactive({
    req(sampling_pops())
    z <- raster::intersect(sampling_points(), admin_boundaries())
    #z <- cbind(z@coords, z@data)
    z <- data.frame(z, sampling_pops())
  })

  ## Process appropriate human population dataset from WorldPop
  dataset_worldpop <- reactive({
    req(survey_area())

    if (input$country == "Tanzania") {
      pop <- raster("www/maps/TZA_popmap10adj_v2b.tif")
      pop <- raster::intersect(pop, survey_area())
    }

    pop
  })

  ## Process appropriate cattle population dataset from GLW
  dataset_glw <- reactive({
    req(survey_area())

    cattle_global <- raster("www/maps/6_Ct_2010_Aw.tif")
    cattle <- raster::intersect(cattle_global, survey_area())
    cattle
  })

  ## Process uploaded human population dataset other than WorldPop
  dataset_human_alt_file_path <- reactive({
    req(input$dataset_input)
    parseDirPath(roots = c("wd" = ".", "home" = "/home"),
                 selection = input$dataset_human_alt)
  })

  dataset_human_pop <- reactive({
    if (input$dataset_type == "gpkg") {
      req(input$dataset_input)
      file <- input$dataset_input
      readOGR(dsn = file$datapath)
    } else {
      req(dataset_human_alt_file_path())
      files <- list.files(dataset_human_alt_file_path())
      layer <- files %>%
        stringr::str_split(pattern = "\\.", simplify = TRUE)
      readOGR(dsn = dataset_human_alt_file_path(),
              layer = layer[1])
    }
  })

  ## Process appropriate cattle population dataset from GLW3
  dataset_cattle_alt_file_path <- reactive({
    req(input$dataset_input)
    parseDirPath(roots = c("wd" = ".", "home" = "/home"),
                 selection = input$dataset_cattle_alt)
  })

  dataset_cattle_pop <- reactive({
    if (input$dataset_type == "gpkg") {
      req(input$dataset_input)
      file <- input$dataset_input
      readOGR(dsn = file$datapath)
    } else {
      req(dataset_cattle_alt_file_path())
      files <- list.files(dataset_cattle_alt_file_path())
      layer <- files %>%
        stringr::str_split(pattern = "\\.", simplify = TRUE)
      readOGR(dsn = dataset_cattle_alt_file_path(),
              layer = layer[1])
    }
  })



  ##############################################################################
  #
  # Process reset calls
  #
  ##############################################################################

  ## Reset maps settings
  observeEvent(input$reset_boundary_settings, {
    shinyjs::reset(id = "boundaries_settings_panel")
  })

  observeEvent(input$reset_centroid_settings, {
    shinyjs::reset(id = "sample_points_settings_panel")
  })

  observeEvent(input$reset_grid_settings, {
    shinyjs::reset(id = "sample_grid_settings_panel")
  })

  ##############################################################################
  #
  # Mapping
  #
  ##############################################################################

  ## Create sample map for settings
  output$sample_base_map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = get(input$base_layer),
                     username = "ernestguevarra") %>%
      setView(lng = 20, lat = 20, zoom = 2)
  })

  ## Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = get(input$base_layer),
        username = "ernestguevarra") %>%
      setView(lng = 20, lat = 20, zoom = 3) %>%
      addScaleBar(position = "bottomright") %>%
      addMeasure(position = "topleft")
  })

  ## Generate administrative borders
  observe({
    req(input$country != " ")

    ## Show progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(
      message = paste("Loading administrative boundaries map of ",
                      input$country, sep = ""),
      value = 0.7)

    leafletProxy("map") %>%
      setView(lng = country_coordinates()[1],
              lat = country_coordinates()[2],
              zoom = 6) %>%
      addPolygons(data = admin_boundaries(),
        color = input$country_boundaries_colour,
        fill = FALSE,
        weight = input$country_boundaries_weight,
        group = "Administrative boundaries")
  })

  ## Should administrative borders be shown?
  observe({
    if (input$show_boundaries) {
      ## Show progress bar
      progress <- Progress$new()
      on.exit(progress$close())
      progress$set(
        message = paste("Loading administrative boundaries map of ",
                        input$country, sep = ""),
        value = 0.7
      )

      leafletProxy("map") %>%
        showGroup("Administrative boundaries")
    } else {
      leafletProxy("map") %>%
        hideGroup("Administrative boundaries")
    }
  })

  ## Add survey area
  observe({
    req(survey_area())
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(
        lng = coordinates(survey_area())[1],
        lat = coordinates(survey_area())[2],
        zoom = 9) %>%
      addPolygons(data = survey_area(),
        color = input$survey_area_colour,
        fill = FALSE,
        weight = input$survey_area_weight,
        group = "Study area") %>%
      addLayersControl(
        overlayGroups = c("Study area"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })

  ## Generate population rasters
  observe({
    req(survey_area(), dataset_worldpop())

    ## Show progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading population raster for study area",
                 value = 0.7)

    leafletProxy("map") %>%
      addRasterImage(
        x = dataset_worldpop(),
        colors = colorNumeric(
          palette = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
          domain = values(dataset_worldpop()),
          na.color = "transparent"),
        opacity = 0.8,
        group = "Human population") %>%
      addLegend(
        title = "Human\npopulation",
        position = "topleft",
        pal = colorNumeric(
          palette = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
          domain = values(dataset_worldpop()),
          na.color = "transparent"),
        values = values(dataset_worldpop()),
        group = "Human population"
      ) %>%
    addLayersControl(
      baseGroups = c("Study area"),
      overlayGroups = c("Human population"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
    )
  })

  ## Generate cattle population rasters
  observe({
    req(survey_area(), dataset_glw())

    ## Show progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading cattle population raster for study area",
                 value = 0.7)

    leafletProxy("map") %>%
      addRasterImage(
        x = dataset_glw(),
        colors = colorNumeric(
          palette = RColorBrewer::brewer.pal(n = 9, name = "YlGnBu"),
          domain = values(dataset_glw()),
          na.color = "transparent"),
        opacity = 0.8,
        group = "Cattle population") %>%
      addLegend(
        title = "Cattle\npopulation",
        position = "topleft",
        pal = colorNumeric(
          palette = RColorBrewer::brewer.pal(n = 9, name = "YlGnBu"),
          domain = values(dataset_glw()),
          na.color = "transparent"),
        values = values(dataset_glw()),
        group = "Cattle population"
      ) %>%
      addLayersControl(
        baseGroups = c("Study area"),
        overlayGroups = c("Human population", "Cattle population"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      ) %>%
      hideGroup(c("Cattle population"))
  })

  ## Add sampling grid
  observeEvent(input$get_sample, {
    leafletProxy("map") %>%
      clearGroup(group = "Sampling points") %>%
      clearGroup(group = "Sampling grid") %>%
      addCircleMarkers(
        lng = sampling_points()@coords[ , 1],
        lat = sampling_points()@coords[ , 2],
        color = input$centroid_colour,
        radius = input$centroid_size,
        weight = input$centroid_weight,
        group = "Sampling points") %>%
      addPolygons(data = sampling_grid(),
        color = input$grid_colour,
        fill = FALSE,
        weight = input$grid_weight,
        group = "Sampling grid") %>%
      addLayersControl(
        baseGroups = c("Study area"),
        overlayGroups = c("Human population", "Cattle population",
                          "Sampling points", "Sampling grid", "Study area"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })

  ## Check if survey area settings are changed
  observeEvent(input$save_boundary_settings, {
    leafletProxy("map") %>%
      clearGroup(group = "Study area") %>%
      addPolygons(data = survey_area(),
        color = input$survey_area_colour,
        fill = FALSE,
        weight = input$survey_area_weight,
        group = "Study area")
  })

  ## Check if centroid settings are changed
  observeEvent(input$save_centroid_settings, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = sampling_points()@coords[ , 1],
        lat = sampling_points()@coords[ , 2],
        color = input$centroid_colour,
        radius = input$centroid_size,
        weight = input$centroid_weight,
        group = "Sampling points")
  })

  ## Check if grid settings are changed
  observeEvent(input$save_grid_settings, {
    leafletProxy("map") %>%
      clearGroup("Sampling grid") %>%
      addPolygons(data = sampling_grid(),
        color = input$grid_colour,
        fill = FALSE,
        weight = input$grid_weight,
        group = "Sampling grid")
  })

  ##############################################################################
  #
  # Reporting
  #
  ##############################################################################

  ts <- reactive({
    Sys.time() %>%
      str_replace(pattern = " ", replacement = "_")
  })

  output$create_report <- downloadHandler(
    filename <- paste(tolower(input$country), ".html", sep = ""),
    content <- function(file) {
      #tempReport <- file.path(tempdir(), "report.Rmd")
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)

      params <- list(country = input$country,
                     nSamplingUnits = input$nSamplingUnits,
                     buffer = input$samplingBuffer,
                     samplingFrame = sampling_points_info(),
                     adminArea = admin_boundaries(),
                     studyArea = survey_area(),
                     samplingGrid = sampling_grid(),
                     samplingPoints = sampling_points(),
                     baseLayer = get(input$base_layer))

      rmarkdown::render(input = "www/report.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )

  observeEvent(input$make_report, {
    ## Show progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(
      message = paste("Rendering sampling frame report for ",
                      input$country, sep = ""),
      value = 0.7
    )

    filename <- paste(tolower(input$country), ".html", sep = "")

    params <- list(ts = ts(),
                   country = input$country,
                   nSamplingUnits = input$nSamplingUnits,
                   buffer = input$samplingBuffer,
                   samplingFrame = sampling_points_info(),
                   adminArea = admin_boundaries(),
                   studyArea = survey_area(),
                   samplingGrid = sampling_grid(),
                   samplingPoints = sampling_points(),
                   baseLayer = get(input$base_layer))

    rmarkdown::render(input = "www/report.Rmd",
                      output_file = filename,
                      output_dir = "www/reports",
                      params = params,
                      envir = new.env(parent = globalenv()))

    browseURL(url = paste("https://aegypti.ecohealthalliance.org/shiny/guevarra/spatialsampling/reports/",
                          tolower(input$country), ".html", sep = ""))

    openxlsx::write.xlsx(
      x = data.frame(coordinates(sampling_points()),
                     sampling_points_info()),
      file = paste("www/reports/", tolower(input$country),
                   "_", ts(), ".xlsx", sep = "")
    )
  })

  output$sample_list <- downloadHandler(
    filename = function() {
      paste(tolower(input$country), "_",
            ts(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(
        x = data.frame(coordinates(sampling_points()),
                     sampling_points_info()),
        file = file)
    }
  )

  ##############################################################################
  #
  # Modal
  #
  ##############################################################################

  ## Landing page modal
  observeEvent(input$inputs_info, {
    showModal(
      modalDialog(
        HTML(
          "<h4>Select study country</h4>
          <p>Select country where study is to be done.</p>
          <p>Please note that once you have selected a country, the application
          will download and read in the map boundary files for the specified
          country. This process can take a bit of time and may make the
          application seem to be unresponsive. Please wait up to a couple of
          minutes for this process to complete before making any other
          inputs or selections. The boundaries of the selected country would
          then be shown on the map once this process is completed.</p>
          <br>
          <h4>Show country boundaries</h4>
          <p>By default, country boundaries are shown on the map once they have
          been downloaded and processed. The country boundaries can be turned
          off by toggling this option.</p>
          <br>
          <h4>Select input file type</h4>
          <p>Select the file type of the input map layer for the study area.
          There are currently only two possible input file types -
          1) shapefiles (SHP); and, 2) geopackage (GPKG).</p>
          <br>
          <h4>Upload shapefile folder of area map</h4>
          <p>If shapefiles is selected as the input file type, click on the
          <strong><em>Upload shapefile folder of area map</em></strong>
          button and a dialog box will appear to select a folder containing
          the various files of a shapefile of the study area.</p>
          <br>
          <h4>Upload geopackage file of area map</h4>
          <p>If geopackage is selected as the input file type, click on the
          <strong><em>Upload geopackage file of area map</em></strong> button
          and a dialog box will appear to select a geopackage file of the
          study area.</p>
          <br>
          <p>Once a map file has been uploaded, this will be read and processed.
          Part of this processing includes the extraction of human and cattle
          population rasters appropriate for the study area. When this process
          has completed, the study area along with the human population raster
          will be shown on the map while the cattle population raster is hidden
          but can be activated via the layers menu on the bottom left side of
          the map.</p>
          <br>
          <h4>Number of sampling units</h4>
          <p>Input the number of sampling units required or desired. For a
          human population study, the recommended minimum number of sampling
          units is at least 30 (default).</p>
          <br>
          <h4>Set a sampling area buffer</h4>
          <p>Sometimes, it is necessary to add a buffer around the study area
          to be able to spread the spatial sample evenly up to the edges. This
          buffer can be specified here. By default, this is set to 0 for no
          buffer. The buffer should be specified in kilometres (kms).</p>
          <br>
          <h4>Sample</h4>
          <p>Once the required number of sampling units has been specified,
          a spatial sample can now be taken. Click this button and a sampling
          grid and sampling points based on the specifications will be added to
          the map.</p>
          <br>
          <h4>List</h4>
          <p>Once the sampling grid and the sampling points have been generated
          on the map, a list of the sampling points and their identifying
          features, geocoordinates, and human and cattle population weights is
          generated and can be saved and downloaded by clicking this button.</p>
          "
        ),
        title = "Spatial inputs and parameters",
        footer = modalButton("Close")
      )
    )
  })

}
