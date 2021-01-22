################################################################################
#
# UI
#
################################################################################

## Set navbar
navbarPage(title = "Spatial Sampling", id = "chosenTab",
  ## Create tab panel
  tabPanel(title = "",
    value = 1,
    icon = icon(name = "home", class = "fa-lg"),
    ## Header HTML
    div(class = "outer",
      ## Header HTML
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      ## Output map
      leafletOutput("map", width = "100%", height = "100%"),
      ## Absolute panel - input
      absolutePanel(id = "controls",
        class = "panel panel-default",
        fixed = TRUE, draggable = FALSE,
        top = 65, left = "auto", right = 10, bottom = "auto",
        width = 330, height = "auto",
        ## UI header
        h4("Sampling inputs and parameters"),
        ##
        selectInput(inputId = "country",
          label = "Study country",
          choices = c("Tanzania", "Rwanda"),
          selected = "Tanzania"
        ),
        ## Input type
        selectInput(inputId = "input_type",
          label = "Select input file type",
          choices = c("shapefile" = "shp",
                      "geopackage" = "gpkg"),
          selected = "shp",
          multiple = FALSE),
        ## Input base maplayer
        uiOutput("survey_area_input"),
        hr(),
        ## Sampling units
        numericInput(inputId = "nSamplingUnits",
          label = "Number of sampling units",
          min = 20, max = 600,
          value = 30, step = 1
        ),
        numericInput(inputId = "samplingBuffer",
          label = "Set an area sampling buffer",
          min = 0, max = 10,
          value = 0, step = 1
        ),
        ## Sampling button
        actionButton(inputId = "get_sample",
          label = "Sample",
          icon = icon(name = "fa-table", lib = "font-awesome")
        )
      )
    )
  )
)


