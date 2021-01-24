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

    ## Report button
    downloadButton(outputId = "create_report",
      label = "Report",
      icon = icon(name = "clipboard", lib = "font-awesome"))
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
                      path = "maps")
    }
  })

  ## Create spatial sample - points
  sampling_points <- eventReactive(input$get_sample, {
    req(input$nSamplingUnits)

    create_sp_grid(x = survey_area(), country = input$country,
                   n = input$nSamplingUnits, buffer = input$samplingBuffer,
                   type = "csas")
  })

  ## Get sampling points information
  sampling_points_info <- reactive({
    req(sampling_points())
    z <- raster::intersect(sampling_points(), admin_boundaries())
    cbind(z@data, z@coords)
  })

  ## Create spatial sample - grid
  sampling_grid <- reactive({
    req(sampling_points())

    sampling_points() %>%
      SpatialPixels() %>%
      as("SpatialPolygons")
  })

  ## Process appropriate human population dataset from WorldPop
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
      setView(lng = country_coordinates()[1],
        lat = country_coordinates()[2],
        zoom = 6)
  })

  ## Generate administrative borders
  observe({
    leafletProxy("map") %>%
      addPolygons(data = admin_boundaries(),
        color = input$country_boundaries_colour,
        fill = FALSE,
        weight = input$country_boundaries_weight,
        group = "Administrative boundaries")
  })

  ## Should administrative borders be shown?
  observe({
    if (input$show_boundaries) {
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
        overlayGroups = c("Sampling points", "Sampling grid", "Study area"),
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

  output$create_report <- downloadHandler(
    filename <- paste(tolower(input$country), ".html", sep = ""),
    content <- function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      params <- list(country = input$country,
                     nSamplingUnits = input$nSamplingUnits,
                     buffer = input$samplingBuffer,
                     samplingFrame = sampling_points_info(),
                     adminArea = admin_boundaries(),
                     studyArea = survey_area(),
                     samplingGrid = sampling_grid(),
                     samplingPoints = sampling_points(),
                     baseLayer = get(input$base_layer))

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}
