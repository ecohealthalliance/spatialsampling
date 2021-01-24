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
    icon = icon(name = "home", class = "fa-lg", lib = "font-awesome"),
    ## Use waiter
    #use_waiter(),
    #waiter_on_busy(html = spin_2(), color = "gray90"),
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
        ## Show admin area map?
        checkboxInput(inputId = "show_boundaries",
          label = "Show country boundaries",
          value = FALSE
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
        ## Report output
        div(style="display:inline-block",
          uiOutput("report_output")
        ),
        ## List output
        div(style="display:inline-block",
          uiOutput("list_output")
        )
      )
    )
  ),
  ## Settings page
  tabPanel(title = "Settings",
    value = 2,
    icon = icon(name = "cog", class = "fa-lg", lib = "font-awesome"),
    tabsetPanel(
      tabPanel(title = "Datasets",
        fluidPage(
          fluidRow(
            ## add empty row for spacing
            column(width = 12, br())
          ),
          fluidRow(
            column(width = 3,
              wellPanel(id = "population_dataset_settings",
                h4("Population"),
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
                h5("Upload population datasets"),
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
            column(width = 3,
              wellPanel(id = "boundary_dataset_settings",
                h4("Country boundaries"),
                HTML("<p>This application uses boundary files available from
                     <a href='https://gadm.org'>https://gadm.org</a>.</p>"),
                br(), br(), br(),
                hr(),
                h5("Upload map boundaries dataset"),
                tags$p("Alternative sources of boundaries datasets can be
                        specified by uploading here."),
                selectInput(inputId = "input_type_boundaries",
                  label = "Select alternative boundaries file type",
                  choices = c("shapefile" = "shp",
                              "geopackage" = "gpkg"),
                  selected = c("shp")
                ),
                uiOutput("boundaries_input"),
                br(), br(), br(), br(), br()
              )
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
              wellPanel(id = "map_layers_settings_panel",
                h4("Base map layers"),
                ## Select base layer
                selectInput(inputId = "base_layer",
                  label = "Select base layer",
                  choices = c("Satellite Street" = "satellite",
                              "Le Shine" = "leShine",
                              "Decimal" = "decimal",
                              "Standard" = "standard",
                              "Northstar" = "northstar",
                              "Terminal" = "teminal",
                              "Moonlight" = "moonlight"),
                  selected = "satellite"
                ),
                hr(),
                ## Show sample of selected base layer
                leafletOutput("sample_base_map", height = 300)
              )
            ),
            column(width = 3,
              wellPanel(id = "boundaries_settings_panel",
                h4("Area boundaries"),
                ## Select colour for study area boundaries
                colourpicker::colourInput(inputId = "survey_area_colour",
                  label = "Colour of survey area boundaries",
                  value = "green",
                  palette = "limited",
                  allowTransparent = TRUE,
                  returnName = TRUE
                ),
                ## Select weight of study area boundaries
                numericInput(inputId = "survey_area_weight",
                  label = "Weigth of survey area boundaries",
                  value = 5,
                  min = 5, max = 20,
                  step = 1
                ),
                hr(),
                ## Select colour for country area boundaries
                colourpicker::colourInput(inputId = "country_boundaries_colour",
                  label = "Colour of country boundaries",
                  value = "yellow",
                  palette = "limited",
                  allowTransparent = TRUE,
                  returnName = TRUE
                ),
                ## Select weight of country area boundaries
                numericInput(inputId = "country_boundaries_weight",
                  label = "Weight of country area boundaries",
                  value = 2,
                  min = 5, max = 20,
                  step = 1
                ),
                hr(),
                ## Reset selection
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "reset_boundary_settings",
                    label = "Reset",
                    class = "btn-primary",
                    icon = icon(name = "refresh",
                                class = "fa-med",
                                lib = "font-awesome"))
                ),
                ## Save centroid settings
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "save_boundary_settings",
                    label = "Save",
                    class = "btn-success",
                    icon = icon(name = "check",
                                class = "fa-med",
                                lib = "font-awesome"))
                )
              )
            ),
            column(width = 3,
              wellPanel(id = "sample_points_settings_panel",
                h4("Sampling points"),
                ## Select colour for sampling points
                colourpicker::colourInput(inputId = "centroid_colour",
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
                br(), br(), br(), br(), br(),
                hr(),
                ## Reset selection
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "reset_centroid_settings",
                    label = "Reset",
                    class = "btn-primary",
                    icon = icon(name = "refresh",
                                class = "fa-med",
                                lib = "font-awesome")
                  )
                ),
                ## Save centroid settings
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "save_centroid_settings",
                    label = "Save",
                    class = "btn-success",
                    icon = icon(name = "check",
                                class = "fa-med",
                                lib = "font-awesome"))
                )
              )
            ),
            column(width = 3,
              wellPanel(id = "sample_grid_settings_panel",
                h4("Sampling grid"),
                ## Select colour for sample grid
                colourpicker::colourInput(inputId = "grid_colour",
                  label = "Colour of sampling grid lines",
                  value = "blue",
                  palette = "limited",
                  allowTransparent = TRUE,
                  returnName = TRUE
                ),
                ## Select weight for sampling grid
                numericInput(inputId = "grid_weight",
                  label = "Weight of sampling grid lines",
                  value = 5,
                  min = 5, max = 10, step = 1
                ),
                br(), br(), br(), br(), br(), br(), br(), br(), br(),
                hr(),
                ## Reset selection
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "reset_grid_settings",
                    label = "Reset",
                    class = "btn-primary",
                    icon = icon(name = "refresh",
                                class = "fa-med",
                                lib = "font-awesome")
                  )
                ),
                ## Save centroid settings
                div(style="display:inline-block; vertical-align:middle;",
                  actionButton(inputId = "save_grid_settings",
                    label = "Save",
                    class = "btn-success",
                    icon = icon(name = "check",
                                class = "fa-med",
                                lib = "font-awesome"))
                )
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
    icon = icon(name = "user", class = "fa-lg", lib = "font-awesome")
  )
)


