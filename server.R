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
  observeEvent(input$reset_centroid_settings, {
    shinyjs::reset(id = "sample_points_settings_panel")
  })


  ##############################################################################
  #
  # Mapping
  #
  ##############################################################################

  ## Get coordinates for selected country
  country_coordinates <- reactive({
    mb_geocode(search_text = input$country)
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

  ## Add survey area
  observeEvent(input$survey_area, {
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      clearMarkers() %>%
      setView(lng = coordinates(survey_area())[1],
              lat = coordinates(survey_area())[2],
              zoom = 9) %>%
      addPolygons(data = survey_area(),
                  color = "yellow",
                  fill = FALSE,
                  weight = 3,
                  group = "survey_area")
  })

  ## Add sampling grid
  observeEvent(input$get_sample, {
    leafletProxy("map") %>%
      clearControls() %>%
      clearGroup(group = "sample_points") %>%
      clearGroup(group = "sample_grid") %>%
      addCircleMarkers(lng = sampling_points()@coords[ , 1],
                       lat = sampling_points()@coords[ , 2],
                       color = "red",
                       radius = input$centroid_size,
                       weight = input$centroid_weight,
                       group = "sample_points") %>%
      addPolygons(data = sampling_grid(),
                  color = "blue",
                  fill = FALSE,
                  weight = 1,
                  group = "sample_grid")
  })
}
