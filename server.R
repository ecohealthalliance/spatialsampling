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

  ## UI for map file input
  output$input_map_file <- renderUI({
    #input$reset_sample_input
    fileInput(inputId = "survey_map",
      label = "Upload file of study area map",
      accept = c(".gpkg", ".zip")
    )
  })

  ## UI for sampling parameters
  output$sample_parameters1 <- renderUI({
    req(input$survey_map)

    ## Sampling units
    numericInput(inputId = "nSamplingUnits",
                 label = "Number of sampling units",
                 min = 20, max = 600,
                 value = 30, step = 1)
  })

  output$sample_parameters2 <- renderUI({
    req(input$survey_map)

    ## Spatial buffer
    numericInput(inputId = "samplingBuffer",
                 label = "Set an area sampling buffer",
                 min = 0, max = 10,
                 value = 0, step = 1)
  })

  output$sample_parameters4 <- renderUI({
    req(input$survey_map)

    checkboxInput(inputId = "centric_sample",
                  label = "Centric sample",
                  value = TRUE)
  })

  output$sample_parameters3 <- renderUI({
    req(input$survey_map)

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

  ## UI for reset sample button
  output$reset_sample <- renderUI({
    req(input$survey_map)

    ## reset button
    actionButton(inputId = "reset_sample_input",
      label = "Reset",
      icon = icon(name = "refresh", lib = "font-awesome")
    )
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

  ## Process input layers
  survey_area <- reactive({
    req(input$survey_map)

    fn <- input$survey_map

    x <- try(readOGR(dsn = fn$datapath))
    #x <- try(as(st_read(dsn = fn$datapath), "Spatial"))

    #if (class(x) == "SpatialMultiPointsDataFrame") {
    #  x <- as(x, "SpatialPointsDataFrame")
    #}

    if (class(x) == "try-error") {
      unzip(zipfile = fn$datapath, exdir = tempdir())
      files <- unzip(zipfile = fn$datapath, list = TRUE)
      layer <- files$Name %>%
        stringr::str_split(pattern = "\\.|\\/", simplify = TRUE)
      #x <- readOGR(dsn = paste0(tempdir(), "/", layer[1, 1]),
      #        layer = layer[2, 2])
      x <- readOGR(dsn = tempdir(), layer = layer[1, 1])
    }

    x
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

    req(survey_area())

    if (!is.null(input$boundary_map)) {
      if (input$input_type_boundaries == "gpkg") {
        req(input$boundary_map)
        file <- input$boundary_map
        z <- readOGR(dsn = file$datapath)
      } else {
        req(boundary_map_file_path())
        files <- list.files(boundary_map_file_path())
        layer <- files %>%
          stringr::str_split(pattern = "\\.", simplify = TRUE)
        z <- readOGR(dsn = boundary_maxp_file_path(),
                     layer = layer[1])
      }
    } else {
      ## Read data from GADM
      country_code <- countrycode(input$country,
                                  origin = "country.name",
                                  destination = "iso3c")

      z <- try(
        raster::getData(
          country = country_code,
          level = 3,
          path = "www/maps"
        )
      )

      if (class(z) == "try-error") {
        z <- raster::getData(
          country = country_code,
          level = 2,
          path = "www/maps"
        )
      }
    }

    ## Subset country boundaries to study area
    z[survey_area(), ]
  })

  ## Create spatial sample - points
  sampling_points_base <- eventReactive(input$get_sample, {
    req(input$nSamplingUnits)

    centric_sample <- create_sp_grid(
      x = survey_area(), country = input$country,
      n = input$nSamplingUnits, buffer = input$samplingBuffer,
      type = "csas"
    )

    sample_grid <- centric_sample %>%
      SpatialPixels() %>%
      as("SpatialPolygons")

    eccentric_sample <- NULL

    for (i in seq_len(length(sample_grid))) {
      current_grid <- sample_grid[i, ]
      sub_grid <- sp::spsample(x = current_grid, n = 25, type = "regular")
      eccentric_sample <- rbind(eccentric_sample, coordinates(sub_grid)[sample(x = 1:25, size = 1), ])
    }

    eccentric_sample <- eccentric_sample %>%
      SpatialPoints(proj4string = CRS(proj4string(survey_area())))

    list(centric_sample, eccentric_sample)
  })

  sampling_points <- reactive({
    ##
    req(sampling_points_base())

    ## Get appropriate sampling_points
    if (input$centric_sample) {
      sampling_points_base()[[1]]
    } else {
      sampling_points_base()[[2]]
    }
  })

  ## Create spatial sample - grid
  sampling_grid <- reactive({
    req(sampling_points_base())

    sampling_points_base()[[1]] %>%
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
    pops1 <- data.frame(matrix(nrow = length(sampling_grid()), ncol = 2))
    names(pops1) <- c("human_pop1", "cattle_pop1")

    for (i in seq_len(length(sampling_grid()))) {
      progress$set(value = i)

      subGridHuman <- raster::intersect(dataset_worldpop(), sampling_grid()[i])
      subGridCattle <- raster::intersect(dataset_glw(), sampling_grid()[i])
      pops1[i, ] <- c(sum(values(subGridHuman), na.rm = TRUE),
                      sum(values(subGridCattle), na.rm = TRUE))
    }

    ## population by points
    pops2 <- data.frame(
      dataset_worldpop()[sampling_points(), ],
      dataset_glw()[sampling_points(), ]
    )

    names(pops2) <- c("human_pop2", "cattle_pop2")

    ## Concatenate
    pops <- data.frame(pops1, pops2)
    pops
  })

  ## Get sampling points information
  sampling_points_info <- reactive({
    req(sampling_pops(), class(sampling_points()) == "SpatialPoints")
    z <- raster::intersect(sampling_points(), admin_boundaries())
    #z <- cbind(z@coords, z@data)
    z <- data.frame(z, sampling_pops())
  })

  ## Process appropriate human population dataset from WorldPop
  dataset_worldpop <- reactive({
    req(survey_area(), input$country != " ")

    ## Progress bar
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(
      message = paste("Retrieving human population raster for ",
                       input$country, sep = ""),
      value = 0.7
    )

    if (input$country == "Tanzania") {
      pop <- try(raster("www/maps/TZA_popmap10adj_v2b.tif"))

      if (class(pop) == "try-error") {
        ccode <- countrycode::countrycode(
          sourcevar = input$country,
          origin = "country.name",
          destination = "iso3c"
        )
        pop <- get_wpgp_pop_data(ccode = ccode, year = 2020)
      }
    } else {
      ccode <- countrycode::countrycode(
        sourcevar = input$country,
        origin = "country.name",
        destination = "iso3c"
      )
      pop <- get_wpgp_pop_data(ccode = ccode, year = 2020)
    }

    raster::intersect(pop, survey_area())
  })

  ## Process appropriate cattle population dataset from GLW
  dataset_glw <- reactive({
    req(survey_area())

    ## Read GLW3 global raster
    cattle_global <- try(raster("www/maps/6_Ct_2010_Aw.tif"))

    if (class(cattle_global) == "try-error") {
      glw_files <- dataverse::get_dataset(
        dataset = "doi:10.7910/DVN/GIVQ75",
        server = "dataverse.harvard.edu")

      cattle_global <- glw_files$files %>%
        dplyr::filter(label == "6_Ct_2010_Aw.tif") %>%
        dplyr::select(id) %>%
        as.vector(mode = "integer") %>%
        dataverse::get_dataframe_by_id(
          .f = raster::raster,
          server = "dataverse.harvard.edu"
        )
    }

    ## Get study are raster
    cattle_local <- raster::intersect(cattle_global, survey_area())

    ## Increase resolution of cattle_local
    cattle_local <- raster::disaggregate(cattle_local, fact = 100)
    values(cattle_local) <- values(cattle_local) / 100

    cattle_local
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

  observeEvent(input$reset_sample_input, {
    shinyjs::refresh()
  })

  ##############################################################################
  #
  # Mapping
  #
  ##############################################################################

  ## Create sample map for settings
  output$sample_base_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
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

    leafletProxy("map") %>%
      setView(lng = country_coordinates()[1],
              lat = country_coordinates()[2],
              zoom = 6)
  })

  ## Add survey area
  observe({
    req(survey_area(), input$country != " ")

    if (class(survey_area()) == "SpatialPolygonsDataFrame") {
      leafletProxy("map") %>%
        clearMarkers() %>%
        #setView(
        #  lng = coordinates(survey_area())[1],
        #  lat = coordinates(survey_area())[2],
        #  zoom = 9) %>%
        fitBounds(
          lng1 = bbox(survey_area())[1, 1],
          lat1 = bbox(survey_area())[2, 1],
          lng2 = bbox(survey_area())[1, 2],
          lat2 = bbox(survey_area())[2, 2]
        ) %>%
        addPolygons(data = admin_boundaries(),
          color = input$country_boundaries_colour,
          fill = FALSE,
          weight = input$country_boundaries_weight,
          group = "Administrative boundaries") %>%
        addPolygons(data = survey_area(),
          color = input$survey_area_colour,
          fill = FALSE,
          weight = input$survey_area_weight,
          group = "Study area") %>%
        addLayersControl(
          baseGroups = c("Administrative boundaries"),
          overlayGroups = c("Study area"),
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        )
    } else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        #setView(
        #  lng = coordinates(survey_area())[1],
        #  lat = coordinates(survey_area())[2],
        #  zoom = 9) %>%
        fitBounds(
          lng1 = bbox(survey_area())[1, 1],
          lat1 = bbox(survey_area())[2, 1],
          lng2 = bbox(survey_area())[1, 2],
          lat2 = bbox(survey_area())[2, 2]
        ) %>%
        addPolygons(data = admin_boundaries(),
                    color = input$country_boundaries_colour,
                    fill = FALSE,
                    weight = input$country_boundaries_weight,
                    group = "Administrative boundaries") %>%
        addCircleMarkers(lng = survey_area()@coords[ , 1],
                         lat = survey_area()@coords[ , 2],
                         color = input$survey_area_colour,
                         radius = input$centroid_size,
                         weight = input$survey_area_weight,
                         group = "Study area") %>%
        addLayersControl(
          baseGroups = c("Administrative boundaries"),
          overlayGroups = c("Study area"),
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        )
    }
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
      clearGroup(group = "Human population") %>%
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
      baseGroups = c("Administrative boundaries"),
      overlayGroups = c("Study area", "Human population"),
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
      clearGroup(group = "Cattle population") %>%
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
        baseGroups = c("Administrative boundaries"),
        overlayGroups = c("Study area", "Human population",
                          "Cattle population"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      ) %>%
      hideGroup(c("Cattle population"))
  })

  ## Add sampling grid
  observeEvent(input$get_sample, {
    if (class(sampling_points()) == "SpatialPoints") {
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
          baseGroups = c("Administrative boundaries"),
          overlayGroups = c("Study area", "Human population", "Cattle population",
                            "Sampling points", "Sampling grid"),
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        )
    } else {
      leafletProxy("map") %>%
        clearGroup(group = "Sampling points") %>%
        clearGroup(group = "Sampling grid") %>%
        addCircleMarkers(
          lng = sampling_points()$coords.x1,
          lat = sampling_points()$coords.x2,
          color = input$centroid_colour,
          radius = input$centroid_size,
          weight = input$centroid_weight,
          group = "Sampling points") %>%
        addLayersControl(
          baseGroups = c("Administrative boundaries"),
          overlayGroups = c("Study area", "Human population", "Cattle population",
                            "Sampling points"),
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        )
    }
  })

  ## Check if sampling inputs are reset
  observeEvent(input$reset_sample_input, {
    leafletProxy("map") %>%
      clearImages() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      removeLayersControl() %>%
      setView(lng = 20, lat = 20, zoom = 3)
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
      if (class(sampling_points()) == "SpatialPoints") {
        openxlsx::write.xlsx(
          x = data.frame(coordinates(sampling_points()),
                         sampling_points_info()),
          file = file)
      } else {
        openxlsx::write.xlsx(
          x = sampling_points(),
          file = file)
      }
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
          country.</p>
          <br>
          <h4>Upload study area map</h4>
          <p>Upload study area map in either shapefiles (SHP) or geopackage
          (GPKG) format. If uploading shapefiles, select a zip file of a folder
          containing the multiple files required.</p>
          <br>
          <p>Once a map file has been uploaded, this will be read and processed.
          Part of this processing includes the downloading of the country
          administrative boundaries, and the extraction of human and cattle
          population rasters appropriate for the study area. When this process
          has completed, the study area and administrative boundaries along with
          the human population raster will be shown on the map while the cattle
          population raster is hidden but can be activated via the layers menu
          on the bottom left side of the map.</p>
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
          <br>
          <h4>Refresh</h4>
          <p>If you want to redo the spatial sampling for some reason or if you
          want to do a spatial sample for a different country or a different
          study area within the same country, click on the refresh button and
          then repeat the steps above.</p>
          "
        ),
        title = "Spatial inputs and parameters",
        footer = modalButton("Close")
      )
    )
  })

}
