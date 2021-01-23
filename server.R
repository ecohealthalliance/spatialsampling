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
    actionButton(inputId = "create_report",
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

  ##############################################################################
  #
  # Process inputs
  #
  ##############################################################################

  ## Process input layer
  survey_area <- reactive({
    if (input$input_type == "gpkg") {
      file <- input$survey_area

      readOGR(dsn = file$datapath)
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

  ## Process appropriate human population dataset from WorldPop


  ## Process appropriate cattle population dataset from GLW3


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

  ## Get coordinates for selected country
  country_coordinates <- reactive({
    mb_geocode(search_text = input$country)
  })

  ## Get boundary files from GADM for chosen country
  admin_boundaries <- reactive({
    country_code <- countrycode(input$country,
      origin = "country.name",
      destination = "iso3c")

    raster::getData(country = country_code, level = 3, path = tempdir())
  })

  ## Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = get(input$base_layer),
        username = "ernestguevarra") %>%
      addPolygons(data = admin_boundaries(),
        color = input$country_boundaries_colour,
        fill = FALSE,
        weight = input$country_boundaries_weight,
        group = "Administrative boundaries") %>%
      addLayersControl(
        baseGroups = c("Administrative boundaries"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      ) %>%
      setView(lng = country_coordinates()[1],
        lat = country_coordinates()[2],
        zoom = 6)
  })

  ## Add survey area
  observeEvent(input$survey_area, {
    leafletProxy("map") %>%
      #clearControls() %>%
      clearMarkers() %>%
      setView(lng = coordinates(survey_area())[1],
        lat = coordinates(survey_area())[2],
        zoom = 9) %>%
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
        baseGroups = c("Administrative Boundaries"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })

  ## Check if mapping settings are changed
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
}
