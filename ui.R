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
          choices = c("Tanzania"),
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
          icon = icon(name = "table", lib = "font-awesome")
        )
      )
    )
  ),
  ## Settings page
  tabPanel(title = "Settings",
    value = 2,
    icon = icon(name = "cog", lib = "font-awesome"),
    tabsetPanel(
      tabPanel(title = "Datasets",
        fluidPage(
          fluidRow(
            ## add empty row for spacing
            column(width = 12, br())
          ),
          fluidRow(
            column(width = 3,
              wellPanel(
                h4("Datasets"),
                hr(),
                tags$p("This application uses pre-loaded population datasets
                       for humans and cattles retrieved from the following
                       sources:"),
                radioButtons(inputId = "pop_data",
                  label = "",
                  choices = c("WorldPop" = "worldpop",
                              "GLW3" = "glw3"),
                  inline = TRUE
                ),
                hr(),
                h5("Upload alternative population datasets"),
                ## Select alternative input
                selectInput(inputId = "dataset_type",
                  label = "Alternative population dataset for",
                  choices = c("humans", "cattles"),
                  selected = "humans"
                ),
                selectInput(inputId = "input_type_alt",
                  label = "Select alternative dataset file type",
                  choices = c("shapefile" = "shp",
                              "geopackage" = "gpkg"),
                  selected = c("shp")
                ),
                uiOutput("dataset_input")
              )
            ),
            column(width = 9,

            )
          )
        )
      ),
      tabPanel(title = "Maps",
        fluidPage(
          fluidRow(
            ## add empty row for spacing
            column(width = 12, br())
          ),
          fluidRow(
            column(width = 3,
              wellPanel(
                h4("Maps"),
                hr()
              )
            )
          )
        )
      )
    )
  ),
  ## About page
  tabPanel(title = "About",
    value = 3,
    icon = icon(name = "user", lib = "font-awesome")
  )
)


