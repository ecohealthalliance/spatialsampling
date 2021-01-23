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

  ## Process appropriate population dataset from WorldPop


  ## Population dataset from GLW3 ----------------------------------------------

  ## Mapping -------------------------------------------------------------------

  ## Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = satellite, username = "ernestguevarra") %>%
      setView(lng = 20, lat = 20, zoom = 4)
  })

  ## Add survey area
  observeEvent(input$survey_area, {
    leafletProxy("map") %>%
      setView(lng = coordinates(survey_area())[1],
              lat = coordinates(survey_area())[2],
              zoom = 9) %>%
      addPolygons(data = survey_area(),
                  color = "yellow",
                  fill = FALSE,
                  weight = 3)
  })

  ## Add sampling grid
  observeEvent(input$get_sample, {
    leafletProxy("map") %>%
      addCircleMarkers(lng = sampling_points()@coords[ , 1],
                       lat = sampling_points()@coords[ , 2],
                       color = "red",
                       radius = 1) %>%
      addPolygons(data = sampling_grid(),
                  color = "blue",
                  fill = FALSE,
                  weight = 1)
  })
}
