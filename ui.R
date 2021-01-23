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
        ## Horizontal line break
        hr(),
        ## Input sampling parameters
        uiOutput("sample_parameters1"),
        uiOutput("sample_parameters2"),
        div(style="display:inline-block",
          uiOutput("sample_parameters3")
        ),
        ##
        div(style="display:inline-block",
          uiOutput("report_output")
        )
      )
    )
  ),
  ## Settings page
  tabPanel(title = "Settings",
    value = 2,
    icon = icon(name = "cog", lib = "font-awesome"),
    tabsetPanel(
      tabPanel(title = "Population",
        fluidPage(
          fluidRow(
            ## add empty row for spacing
            column(width = 12, br())
          ),
          fluidRow(
            column(width = 3,
              wellPanel(
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
                tags$p("Alternative sources of population datasets for humans
                       and/or cattle can be specified by uploading these
                       sources here."),
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
          useShinyjs(),
          fluidRow(
            ## add empty row for horizontal spacing
            column(width = 12, br())
          ),
          fluidRow(
            column(width = 3,
              wellPanel(
                h4("Settings for map layers"),
                ## Select base layer
                selectInput(inputId = "base_layer",
                  label = "Select base layer",
                  choices = c("Satellite Street" = "satellite",
                              "Le Shine" = "leShine",
                              "Decimal" = "decimal",
                              "Standard" = "standard",
                              "Northstar" = "northstar"),
                  selected = "satellite"
                )
              )
            ),
            column(width = 3,
              wellPanel(
                id = "sample_points_settings_panel",
                h4("Settings for sampling points"),
                ## Select colour for sampling points
                colourpicker::colourInput(inputId = "centroid_colours",
                  label = "Colour of sampling points",
                  value = "red",
                  palette = "limited",
                  allowTransparent = TRUE,
                  returnName = TRUE
                ),
                ## Select size for sampling points
                numericInput(inputId = "centroid_size",
                  label = "Size of sampling points",
                  value = 2,
                  min = 1, max = 10, step = 1
                ),
                ## Select weight for sampling points
                numericInput(inputId = "centroid_weight",
                  label = "Weight of sampling points",
                  value = 5,
                  min = 5, max = 10, step = 1
                ),
                ## Reset selection
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "reset_centroid_settings",
                    label = "Reset",
                    class = "btn-primary",
                    icon = icon(name = "refresh", lib = "font-awesome")
                  )
                ),
                ##
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "save_centroid_colours",
                    label = "Save",
                    class = "btn-success",
                    icon = icon(name = "check", lib = "font-awesome"))
                ),
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


