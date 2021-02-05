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
        div(style="display: inline-block;vertical-align: middle;",
          h4("Sampling inputs and parameters")
        ),
        ## Action link for modal
        div(style="display: inline-block;vertical-align: middle;",
          actionLink(inputId = "inputs_info",
            label = "",
            icon = icon(name = "info-sign", lib = "glyphicon")
          )
        ),
        ##
        selectInput(inputId = "country",
          label = "Study country",
          choices = c("Select study country" = " ", "Tanzania"),
          selected = " "
        ),
        ## Show admin area map?
        checkboxInput(inputId = "show_boundaries",
          label = "Show country boundaries",
          value = TRUE
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
        #div(style="display:inline-block",
        #  uiOutput("report_output")
        #),
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
    icon = icon(name = "info-circle", class = "fa-lg", lib = "font-awesome"),
    HTML(
      "<h2>Spatial sampling</h2>
      <p>Area sampling approaches such as <strong>centric systematic area
      sampling</strong> or <strong>CSAS</strong> have potential applications in
      studies that sample from mixed human and animal subjects, and from the
      environment. Standard sampling approaches that select subjects
      <em>proportional to population size (PPS)</em> are impractical for use in
      these studies because sampling units selected for one subject will not
      necessarily be representative of the other subjects. This is particularly
      true in contexts where animal population size distribution is inversely
      related to human population size.</p>
      <br>
      <p>With <strong>CSAS</strong>, sampling of any subject is performed
      systematically over geographic area and resulting sample is said to be
      spatially representative. This type of sample is approximates a
      <strong>simple random sample (SRS)</strong><sup>1</sup>. Thus, a mixed
      human and animal subject study can use the same spatial sampling frame
      for all subjects. Additional advantages of a <strong>CSAS</strong>
      approach is that the resulting sample is implicitly spatially stratified
      which contributes to increased sampling variance<sup>2,3</sup>.</p>
      <br>
      <h3>Steps in CSAS</h3>
      <br>
      <h4>Step 1: Find a map</h4>
      <p>The first step is to find a map of the study area. Try to find a map
      with as much detail and features (e.g., towns, villages, landmarks, etc)
      as possible.</p>
      <br>
      <h4>Step 2: Draw a grid</h4>
      <p>The size of each square should be small enough for it to be reasonable
      to assume homogeneity within the square. The size of the grid will also
      be dictated by a target number of sampling units that you are aiming
      for.</p>
      <br>
      <h4>Step 3: Select the areas to sample</h4>
      <p>The sampling points which are the centroids of the sampling grid points
      us where to sample. For human and animal populations, this can be the
      village or settlement at or near the centroid location. For environmental
      sampling, this would be specimens drawn at or near the centroid
      locations.</p>
      <br>
      <h3>Data analysis considerations</h3>
      <p>Data collected from this type of sampling frame can the be made
      population representative by applying a population weighted analysis
      during indicator/outcome estimation. This can be done parametrically
      using Taylor linearised deviation techniques (which can be implemented in
      <a href='https://cran.r-project.org' target='_blank'>R</a> using Thomas
      Lumley's <a href='https://cran.r-project.org/web/packages/survey/survey.pdf'
      target='_blank'>survey package</a><sup>4</sup> or using a non-parametric
      weighted bootstrap approach such as the one described
      <a href='https://github.com/rapidsurvys/bbw' target='_blank'>here</a>
      <sup>5</sup>.</p>
      <br>
      <h3>About the spatialsampling Shiny application</h3>
      <p>This <a href='http://shiny.rstudio.com' target='_blank'>Shiny</a>
      application assists users in the process of applying <strong>CSAS</strong>
      to a specified study area. This application utilises the
      <a href='https://cran.r-project.org' target='_blank'>R</a> package
      <a href='https://github.com/spatialworks/spatialsampler'
      target='_blank'>spatialsampler</a><sup>6</sup> which provides functions
      for performing <strong>CSAS</strong>.</p>
      <br>
      <h3>References</h3>
      <p><sup>1</sup> Milne, A. (1959). The Centric Systematic Area-Sample
      Treated as a Random Sample. Biometrics, 15(2), 270-297.
      <a href='https://doi.org/10.2307/2527674'
      target='_blank'>https://doi.org/10.2307/2527674</a></p>
      <p><sup>2</sup> Aaron GJ, Strutt N, Boateng NA, Guevarra E, Siling K,
      et al. (2016) Assessing Program Coverage of Two Approaches to
      Distributing a Complementary Feeding Supplement to Infants and Young
      Children in Ghana. PLOS ONE 11(10): e0162462.
      <a href='https://doi.org/10.1371/journal.pone.0162462' target='_blank'>
      https://doi.org/10.1371/journal.pone.0162462</a></p>
      <p><sup>3</sup> Aaron, G. J. et al. (2016) ‘Household coverage of fortified
      staple food commodities in Rajasthan, India’, PLoS ONE, 11(10).
      <a href='https://doi.org/10.1371/journal.pone.0163176' target='_blank'>
      https://doi.org/10.1371/journal.pone.0163176</a></p>
      <p><sup>4</sup> Lumley T. Analysis of complex survey samples. Journal of
      Statistical Software. 2004;9: 1–19. Available:
      <a href='http://www.jstatsoft.org/v09/a08/paper' target='_blank'>
      http://www.jstatsoft.org/v09/a08/paper</a></p>
      <p><sup>5</sup> Mark Myatt (2018). bbw: Blocked Weighted Bootstrap. R
      package version 0.1.3. <a href='https://CRAN.R-project.org/package=bbw'
      target='_blank'>https://CRAN.R-project.org/package=bbw</a></p>
      <p><sup>6</sup> Mark Myatt, Farah Ibrahim and Ernest Guevarra (2021).
      spatialsampler: An Implementation of the Centric Systematic Area Sampling
      (CSAS) and Simple Spatial Sampling Method (S3M) sampling approaches in R.
      R package version 0.1.0. <a href='https://github.com/spatialworks/spatialsampler'
      target='_blank'>https://github.com/spatialworks/spatialsampler</a></p>
      "
    )
  )
)


