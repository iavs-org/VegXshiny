#' tableImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import bslib

mod_tableImport_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    tabsetPanel(
      tabPanel("Table to Veg-X",
        tagList(
          shinyjs::useShinyjs(),
          
          tags$head(
            tags$style(HTML("
             .col-sm-3 {
                width: 200px;
                margin-top: 35px;
             }
           "))
          ),
          navset_pill_list(
            id=ns("sidebar"),
            widths = c(3, 9),
            selected = "Project",
            
            # Project ####
            nav_panel(title = "1. Project", value = "Project",
                column(
                  width = 12,
                  h1("Project"),
                  tags$p("Describe your project and its contributors. Files can be assigned in the next steps", class = "text-info annotation no-margin"),
                  hr(),
                  uiOutput(ns("project_ui"))
                )
            ),
            
            # Plots ####
            nav_panel(title = "2. Plots", value = "Plots",
                column(
                  width = 12,
                  h1("Plots"),
                  tags$p("Describe static plot properties. Properties of observations (e.g. date, observer) can be assigned in the next step.", class = "text-info annotation no-margin"),
                  hr(),
                  uiOutput(ns("plot_ui"))
                )
            ),
            
            
            # Observations ####
            nav_panel(title = "3. Observations", value = "Observations",  
                column(
                  width = 12,
                  h1("Observations"),
                  tags$p("Import your observation data", class = "text-info annotation no-margin"),
                  hr(),
                  tags$h4("Observation categories"),
                  checkboxGroupInput(ns("observations_input_control"), label = NULL, inline = T, 
                                     choiceNames = c("Individual organisms", "Aggregate organisms", "Stratum", "Community", "Surface cover"),
                                     choiceValues = c("individualOrganismObservations", "aggregateOrganismObservations", 
                                                      "stratumObservations", "communityObservations", "surfaceCoverObservations")),
                  
                  tags$div(
                    id = ns("observationsAccordion"), class = "panel-group", "role" = "tablist",
                    tags$div(
                      id = ns("individualOrganismObservations"),
                      class = "panel panel-default",
                      tags$div(
                        id = ns("individualOrganismObservationsHeading"), class = "panel-heading" , "role" = "tab",
                        tags$h4(
                          class = "panel-title",
                          tags$a("IndividualOrganismObservations", class = "collapsed",
                                 "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("individualOrganismObservationsBody"))
                          )
                        )
                      ),
                      tags$div(
                        id = ns("individualOrganismObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                        tags$div(
                          class = "panel-body",
                          uiOutput(ns("individualOrganismObservations_ui"))
                        )
                      )
                    ),
                    
                    tags$div(
                      id = ns("aggregateOrganismObservations"),
                      class = "panel panel-default",
                      tags$div(
                        id = ns("aggregateOrganismObservationsHeading"), class = "panel-heading" , "role" = "tab",
                        tags$h4(
                          class = "panel-title",
                          tags$a("AggregateOrganismObservations", class = "collapsed",
                                 "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("aggregateOrganismObservationsBody"))
                          ),
                          tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "An observation applying to all occurrences of an organism during a plot observation based on an aggregation factor, e.g. a measurement of the overall cover/biomass/etc. of a specific taxon. Further stratification of the observation by vegetation layer and subplot is possible.")
                        ),
                      ),
                      tags$div(
                        id = ns("aggregateOrganismObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                        tags$div(
                          class = "panel-body",
                          uiOutput(ns("aggOrgObs_ui"))
                        )
                      )
                    ),
                    
                    tags$div(
                      id = ns("stratumObservations"),
                      class = "panel panel-default",
                      tags$div(
                        id = ns("stratumObservationsHeading"), class = "panel-heading" , "role" = "tab",
                        tags$h4(
                          class = "panel-title",
                          tags$a("StratumObservations", class = "collapsed",
                                 "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("stratumObservationsBody"))
                          ),
                          tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "An observation applying to an entire stratum during a plot observation, e.g. a measurement of the total cover of the herb layer. Note that taxon-specific stratum observations such as abundance estimates for a taxon in a specific layer can be imported under aggregateOrganismObservations.")
                        )
                      ),
                      tags$div(
                        id = ns("stratumObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                        tags$div(
                          class = "panel-body",
                          uiOutput(ns("stratumObs_ui"))
                        )
                      )
                    ),
                    
                    tags$div(
                      id = ns("communityObservations"),
                      class = "panel panel-default",
                      tags$div(
                        id = ns("communityObservationsHeading"), class = "panel-heading" , "role" = "tab",
                        tags$h4(
                          class = "panel-title",
                          tags$a("CommunityObservations", class = "collapsed",
                                 "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("communityObservationsBody"))
                          )
                        )
                      ),
                      tags$div(
                        id = ns("communityObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                        tags$div(
                          class = "panel-body",
                          uiOutput(ns("communityObservations_ui"))
                        )
                      )
                    ),
                    
                    tags$div(
                      id = ns("surfaceCoverObservations"),
                      class = "panel panel-default",
                      tags$div(
                        id = ns("surfaceCoverObservationsHeading"), class = "panel-heading" , "role" = "tab",
                        tags$h4(
                          class = "panel-title",
                          tags$a("SurfaceCoverObservations", class = "collapsed",
                                 "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("surfaceCoverObservationsBody"))
                          ),
                          tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "An observation applying to different surface types within a plot. Further stratification of the observation by subplot is possible.")
                        )
                      ),
                      tags$div(
                        id = ns("surfaceCoverObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                        tags$div(
                          class = "panel-body",
                          uiOutput(ns("covObs_ui"))
                        )
                      )
                    )
                  )
                )
            ),
            
            # Summary ####
            nav_panel(title = "4. Summary", value = "Summary",
                column(
                  width = 12,
                  h1("Summary"),
                  tags$p("Check your entries", class = "text-info annotation no-margin"),
                  hr(),
                  
                  h3("Project", style = "margin-bottom: 6px"),
                  fluidRow(
                    column(12,
                           uiOutput(ns("summary_project"))
                    )
                  ),
                  
                  h3("Plots"),
                  fluidRow(
                    column(12,
                           uiOutput(ns("summary_plot_id")),
                           uiOutput(ns("summary_plot_coordinates")),
                           uiOutput(ns("summary_plot_elevation")),
                           uiOutput(ns("summary_plot_geometry")),
                           uiOutput(ns("summary_subplot_geometry")),
                           uiOutput(ns("summary_plot_topography")),
                           uiOutput(ns("summary_plot_parent_material"))
                    )
                  ),
                  
                  h3("Observations"),
                  fluidRow(
                    column(12,
                           uiOutput(ns("summary_observations"))
                    )
                  )
                )
            )
          ),
          
          # Navigation bar ####
          div(
            uiOutput(ns("navigation_ui")),
            style = "margin-bottom: 100px"
          )
        )
      ),
      tabPanel("Help",
        div(
          class = "content",
          tags$h1("Help with reading tabular data"),
          div(class = "info-box",
             div(class = "text-info info-box-item",
                 icon("lightbulb", class = "icon-padded-right"),
                 tags$span(style = "font-size:1.8rem;", "Before data can be read 
                 in here, it must first be uploaded in the 'Start' section")),
          ),
          tags$p("Tabular data for import are expected to be in a format 
                  where each observation has its row and each variable has its
                  column. One consequence is that a transformation is 
                  required for 'header data' following a convention where
                  variables such as date of recording or elevation are 
                  organized in rows. A valid 'header' table has at least one 
                  column with plot names (as far as static plot attributes like
                  spatial coordinates are concerned) and one additional column
                  with observation dates in case of non-static attributes
                  like species cover. This follows the Veg-X
                  logic. Due to limitations of the underlying packages, a
                  date column needs to be prepared outside this app if it is
                  lacking. A valid species table has a column for plot IDs, a
                  column for observation date, a column for species names and
                  one for the cover or count values. If species aggregation took
                  place in layers, an additional column for layers is needed."),
         tags$p("The tools provided in the preparation step help with the          
                  transformations. The import function guides step by step
                  towards the goal. Details on the expected format of input data
                  is available when hovering the info icon next to the dataset
                  selection in the table import dialog."),
         tags$p("The import of tabular data is structured into five steps. 
                  Mandatory fields are marked with a star (*)"),
         tags$ol(
           tags$p(tags$li("Project information")),
           tags$p("Provide a general description of the project and its 
                  contributors. No files need to be assigned in this step."),
           tags$p(tags$li("Plot information")),
           tags$p("Describe static plot properties (like surface area of the 
                   plot. This is distinguished from features that belong 
                   to a plotObservation, such as the date of a recording). 
                   The expected data set contains one plot per row. Each row is 
                   identified by a unique plot ID (mandatory) and may contain 
                   static plot properties (optional):"),
           tableOutput(ns("example_df_plots")),
           br(),
           tags$p("Each mapped plot property needs to be linked to a 
                  measurement method. Prefabricated descriptions of
                  common measurement methods are available in the corresponding 
                  mapping dialog. For example, if plot area was measured in 
                  square metres, selecting \"Plot area/m2\" as measurement 
                  method will define an appropriate method and link it to the 
                  measurement values during import. If none of the predefined
                  methods is applicable, a custom method can be defined by 
                  selecting the \"...add custom method\" option."),
           
           tags$p(tags$li("Observations")),
           tags$p("Add different types of observations, such as species
                   cover, vegetation layers, or soil properties. All observations 
                   in Veg-X refer to a plotObservation, i.e. a sampling event at 
                   a specific plot at a specific point in time. Thus, 
                   ", tags$span("observation data of any type are identified at 
                   least by a unique combination of plot ID and date. ", 
                                class = "text-info"), "Depending on the observation type, 
                   other attributes are required. For example, the import of 
                   measured species cover values (called 
                   aggregateOrganismObservations), require a dataset with at 
                   least four columns as shown in this example: "),
           tableOutput(ns("example_df_obs")),
           br(),
           tags$p("This is an example for a 'long table', - the 
                   species coverages are often stored differently, i.e. a 
                   transformation is often necessary before importing (there 
                   are tools available to help you with that).  If the 
                   aggregateOrganismObservations were made at a more granular 
                   level, e.g. for separate subplots or vegetation strata, 
                   additional data columns are needed."),
           tags$p("As in the case of plot properties, measurements of 
                   observations require defined measurement methods. A dropdown 
                   menu provides a list of predefined methods. If none of these  
                   predefined methods is applicable, a custom method can be 
                   created by selecting the \"...add custom method\" option."),
           tags$p("Note that currently VegXshiny does not support imports into 
                   the Veg-X container for measurements applying to an entire 
                   plant community, such as successional stage (called 
                   communityObservations). The same applies to measurements 
                   related to individual organisms, such as DBH (called 
                   individualOrganismObservations)."),
           tags$p(tags$li("Check inputs")),
           tags$p("Check your mapped inputs before starting the import. 
                   Color-coded boxes indicate the status:"),
           tags$ul(style = "margin-bottom: 10px",
                   tags$li(tags$span("Green", style = "color: #00a97e"), 
                           ": Inputs complete, ready for import."),
                   tags$li(tags$span("Red", style = "color: #da3b3b"), 
                           ": Inputs incomplete, review before import."),
                   tags$li(tags$span("Grey", style = "color: #666666"), 
                           ": Inputs incomplete, ignored during import."),
           ),
           tags$p("Only when all input categories are marked either green or 
                   grey, you will be able to proceed with the import."),
           
           tags$p(tags$li("Import")),
           tags$p("Pressing", tags$i("Import"), "and confirming the dialog will 
                   create a new Veg-X document from the specified mappings. ", 
                  tags$span("Your current Veg-X document will be overwritten by 
                   this. ", class = "text-info")),
           tags$p("Note that the import may take a while when working with large 
                   input files containing thousands of observations. A progress 
                   bar in the lower left corner indicates the current status. 
                   Once the import is finished, you can view the imported Veg-X 
                   file in the ", tags$i("Veg-X viewer"), " or export it under 
                   ", tags$i("Veg-X export."))
         ),

        )
      )      
    )
  )
}

#' tableImport Server Functions
#'
#' @noRd 
mod_tableImport_server <- function(id, file_order, user_data, vegx_schema, vegx_doc, vegx_txt, templates, templates_lookup, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Observers & reactives ####
    # Pull method templates
    methods = reactive({
      list(location = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "location") %>% arrange(name) %>% pull(template_id, name),
           elevation = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "elevation") %>% arrange(name) %>% pull(template_id, name),
           plot_dimension = templates_lookup()  %>% dplyr::filter(target_element == "methods", subject == "plot dimension") %>% arrange(name) %>% pull(template_id, name),
           plot_area = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "plot area") %>% arrange(name) %>% pull(template_id, name),
           aspect = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "aspect") %>% arrange(name) %>% pull(template_id, name),
           slope = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "slope") %>% arrange(name) %>% pull(template_id, name),
           aggOrgObs = templates_lookup() %>% filter(target_element == "methods", subject %in% c("plant cover", "plant count", "plant frequency", "basal area", "user-defined aggregate measurement")) %>% arrange(name) %>% pull(template_id, name),
           strataDef = templates_lookup() %>% filter(target_element == "strata", subject == "strata definition") %>% arrange(name) %>% pull(template_id, name),
           stratumObs = templates_lookup() %>% filter(target_element == "methods", subject %in% c("plant cover", "user-defined stratum measurement")) %>% arrange(name) %>% pull(template_id, name),
           covObs = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "surface cover") %>% arrange(name) %>% pull(template_id, name)
      )
    })
    
    # Observe method inputs and trigger mod_new*Template when custom template option is selected
    observe_method_input = function(input_name, method_subject, template_type = c("method", "strataDef")){
      if(input[[input_name]] == "custom_template"){
        module_id = paste0("new_method-", sample.int(1000000, 1)) # Create a random unique ID
        if(template_type == "method"){
          mod_newMethodTemplate_server(module_id, method_subject, templates, templates_lookup)
          mod_newMethodTemplate_ui(ns(module_id))
        } else if(template_type == "strataDef"){
          mod_newStrataDefTemplate_server(module_id, method_subject, templates, templates_lookup)
          mod_newStrataDefTemplate_ui(ns(module_id))
        }
        updateSelectizeInput(session, input_name, selected = "")
      }
    }
    
    observeEvent(input$plot_location_method, handlerExpr = observe_method_input("plot_location_method", "location", "method"))
    observeEvent(input$plot_elevation_method, handlerExpr = observe_method_input("plot_elevation_method", "elevation", "method"))
    observeEvent(input$plot_dimensions_method, handlerExpr = observe_method_input("plot_dimensions_method", "plot dimension", "method"))
    observeEvent(input$plot_area_method, handlerExpr = observe_method_input("plot_area_method", "plot area", "method"))
    observeEvent(input$plot_aspect_method, handlerExpr = observe_method_input("plot_aspect_method", "aspect", "method"))
    observeEvent(input$plot_slope_method, handlerExpr = observe_method_input("plot_slope_method", "slope", "method"))
    observeEvent(input$subplot_dimensions_method, handlerExpr = observe_method_input("subplot_dimensions_method", "plot dimension", "method"))
    observeEvent(input$subplot_area_method, handlerExpr = observe_method_input("subplot_area_method", "plot area", "method"))
    observeEvent(input$aggOrgObs_strataDef, handlerExpr = observe_method_input("aggOrgObs_strataDef", "strata definition", "strataDef"))
    observeEvent(input$aggOrgObs_measurementScale, handlerExpr = observe_method_input("aggOrgObs_measurementScale", "user-defined aggregate measurement", "method"))
    observeEvent(input$stratumObs_strataDef, handlerExpr = observe_method_input("stratumObs_strataDef", "strata definition", "strataDef"))
    observeEvent(input$stratumObs_measurementScale, handlerExpr = observe_method_input("stratumObs_measurementScale", "user-defined stratum measurement", "method"))
    observeEvent(input$covObs_measurementScale, handlerExpr = observe_method_input("covObs_measurementScale", "surface cover", "method"))
    
    # Update method inputs to prevent re-rendering of the entire UI when `methods()` changes
    observeEvent(  
      eventExpr = methods(),
      handlerExpr = {
        inputs = data.frame(input_id = c("plot_location_method", "plot_elevation_method", "plot_dimensions_method", "plot_area_method",
                                         "plot_aspect_method", "plot_slope_method", "subplot_dimensions_method", "subplot_area_method",
                                         "aggOrgObs_strataDef", "aggOrgObs_measurementScale", "stratumObs_strataDef", "stratumObs_measurementScale",
                                         "covObs_measurementScale"),
                            method_name = c("location", "elevation", "plot_dimension", "plot_area", 
                                            "aspect", "slope", "plot_dimension", "plot_area", 
                                            "strataDef", "aggOrgObs", "strataDef", "stratumObs", 
                                            "covObs")
        )
        
        for(i in 1:nrow(inputs)){
          id = inputs$input_id[i]
          method = inputs$method_name[i]
          updateSelectizeInput(session, inputId = id, selected = input[[id]], choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(methods()[[method]]), after = 1))
        }
      }
    ) 
    
    # Dynamic display text for data dropdown menus
    dropdown_empty = reactive({
      user_data = isolate(user_data)
      if(length(names(user_data)) == 0){
        c("No files found" = "")
      } else {
        c("Choose a file" = "")
      }
    })
    
    #-------------------------------------------------------------------------#
    # Navigation ####
    sidebar_tabs = c("Project", "Plots", "Observations", "Summary")
    
    output$navigation_ui = renderUI({
      if(input$sidebar == "Data"){
        buttons = fluidRow(
          column(width = 12, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right"))
        )
      } else if(input$sidebar =="Summary") {
        buttons = fluidRow(
          column(width = 2, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
          column(width = 2, offset = 8, actionButton(ns("import"), label = "Import", width = "100px", class = "btn-success pull-right"))
        )  
      } else {
        buttons = fluidRow(
          column(width = 2, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
          column(width = 2, offset = 8, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right"))
        )
      }
      
      fluidRow(
        column(3),  
        column(
          9,
          column(
            width = 12,
            hr(),
            buttons
          )
        )
      )      
    })
    
    observeEvent(input$previous_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) - 1]))
    observeEvent(input$next_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) + 1]))
    
    #-------------------------------------------------------------------------#
    # Project ####
    output$project_ui = renderUI({
      tagList(
        textInput(inputId = ns("project_title"), label = "Project title *",  width = "100%"),
        textAreaInput(inputId = ns("project_abstract"), label = "Abstract", width = "100%", resize = "vertical"),
        textInput(inputId = ns("project_citation"), label = "Citation", width = "100%"),
        tags$label("Responsible Party"),
        fluidRow(
          column(width = 4, textInput(ns("party_name"), "Name", width = "100%")),
          column(width = 4, textInput(ns("party_role"), "Role", width = "100%")),
          column(width = 4, selectizeInput(ns("party_type"), label = "Type", choices = c("", "Individual", "Organization", "Position"), width = "100%"))
        )
      )
    })
    
    #-------------------------------------------------------------------------#
    # Plots ####
    # Observe and update data inputs instead of using a reactive expression in their definition, thus preventing re-rendering of the entire UI when `user_data()` changes 
    observe({  
      if(!is.null(input$plot_data)){
        if(input$plot_data %in% file_order()){
          file_selected = input$plot_data # save current selection
          choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
          updateSelectizeInput(session, inputId = "plot_data", selected = file_selected, choices = c(dropdown_empty(), choices))  
        } else {
          choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
          updateSelectizeInput(session, inputId = "plot_data", selected = NULL, choices = c(dropdown_empty(), choices))
        }
      }
      
      if(!is.null(input$subplot_data)){
        if(input$plot_hasSubplot == "yes" && input$subplot_data %in% file_order()){
          file_selected = input$subplot_data # save current selection
          choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
          updateSelectizeInput(session, inputId = "subplot_data", selected = file_selected, choices = c(dropdown_empty(), choices))
        } else {
          choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
          updateSelectizeInput(session, inputId = "subplot_data", selected = NULL, choices = c(dropdown_empty(), choices))
        }
      } 
    })
    
    output$plot_ui = renderUI({
      tagList(
        tags$h4("Main plots"),
        tags$p("Assign a dataset", class = "text-info annotation"),
        tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A wide-format table with one row per main plot and additional plot properties in columns"),
        selectizeInput(ns("plot_data"), label = NULL, choices = c("No files found" = "")),
        uiOutput(ns("plot_mappings_ui"))
      )
    })
    
    output$plot_mappings_ui = renderUI({
      req(input$plot_data)
      tagList(
        selectizeInput(inputId = ns("plot_unique_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders)),
        
        checkboxGroupInput(ns("plot_input_control"), label = "Additional plot information", inline = T,
                           choiceNames = c("Coordinates", "Elevation", "Geometry", "Topography", "Parent Material"),
                           choiceValues = c("Coordinates", "Elevation", "Geometry", "Topography", "ParentMaterial")),
        
        tags$div(
          id = ns("plotAccordion"), class = "panel-group", "role" = "tablist",
          
          ## Coordinates ####
          shinyjs::hidden(tags$div(
            id = ns("plotCoordinates"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotCoordinatesHeading") , class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Coordinates",
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotAccordion")), "href"=paste0("#", ns("plotCoordinatesBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotCoordinatesBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("plot_coordinates_x"), label = "X-Coordinate", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))), 
                  column(4, selectizeInput(ns("plot_coordinates_y"), label = "Y-Coordinate", choices =  c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_location_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$location), after = 1)))
                ), 
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(12, textInput(ns("plot_crs"), label = "Coordinate reference string (CRS)", width = "100%"))
                )
              )
            )
          )),
          
          # Elevation ####
          shinyjs::hidden(tags$div(
            id = ns("plotElevation"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotElevationHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Elevation",
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotAccordion")), "href"=paste0("#", ns("plotElevationBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotElevationBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("plot_elevation"), label = "Elevation", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),  
                  column(4, selectizeInput(ns("plot_elevation_method"), label = "Measurement method",
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$elevation), after = 1)))
                )
              )
            )
          )),
          
          # Geometry ####
          shinyjs::hidden(tags$div(
            id = ns("plotGeometry"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotGeometryHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Geometry", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotAccordion")), "href"=paste0("#", ns("plotGeometryBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotGeometryBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("plot_shape"), label = "Shape", choices =  c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectizeInput(ns("plot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_dimensions_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$plot_dimension), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectizeInput(ns("plot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_area_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$plot_area), after = 1)))
                )
              )
            )
          )),
          
          # Topography ####
          shinyjs::hidden(tags$div(
            id = ns("plotTopography"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotTopographyHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Topography", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotAccordion")), "href"=paste0("#", ns("plotTopographyBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotTopographyBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("plot_aspect"), label = "Aspect", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_aspect_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$aspect), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectizeInput(ns("plot_slope"), label = "Slope", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("plot_slope_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$slope), after = 1)))
                )
              )
            )
          )),
          # Parent material ####
          shinyjs::hidden(tags$div(
            id = ns("plotParentMaterial"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotParentMaterialHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Parent Material", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotAccordion")), "href"=paste0("#", ns("plotParentMaterialBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotParentMaterialBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("plot_parent_material"), label = "Parent material", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                )
              )
            )
          )
          )),
        hr(),
        tags$h4("Subplots"),
        tags$p("Are plot structured into subplot?", class = "text-info annotation"),
        radioButtons(ns("plot_hasSubplot"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T),
        uiOutput(ns("subplot_ui")),
      )
    })
    
    observe({
      panel_names =  c("Coordinates", "Elevation", "Geometry", "Topography", "ParentMaterial")
      for(panel_name in panel_names){
        if(panel_name %in% input$plot_input_control){
          shinyjs::show(paste0("plot", panel_name))
        } else {
          shinyjs::hide(paste0("plot", panel_name))
        }
      }
    })
    
    # Subplots ####
    output$subplot_ui = renderUI({
      if(input$plot_hasSubplot == "yes"){
        tagList(
          tags$p("Assign a dataset", class = "text-info annotation"),
          tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A wide-format table with one row per subplot (identified by unique combinations of plot and subplot ids) and additional subplot properties in columns"),
          selectizeInput(ns("subplot_data"), label = NULL, choices = list("Choose a file" = "")), 
          uiOutput(ns("subplot_mappings_ui"))
        )
      }
    })
    
    output$subplot_mappings_ui = renderUI({
      req(input$subplot_data)
      
      tagList(
        fluidRow(
          column(4, selectizeInput(inputId = ns("subplot_plot_unique_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
          column(4, selectizeInput(inputId = ns("subplot_id"), label = "Subplot ID *", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders)))
        ),
        checkboxGroupInput(ns("subplot_input_control"), label = "Additional sub-plot information", inline = T, choices = "Geometry"),
        
        tags$div(
          id = ns("subplotAccordion"), class = "panel-group", "role" = "tablist",
          # Geometry ####
          shinyjs::hidden(tags$div(
            id = ns("subplotGeometry"),
            class = "panel panel-default",
            tags$div(
              id = ns("subplotGeometryHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Geometry", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("subplotAccordion")), "href"=paste0("#", ns("subplotGeometryBody"))
                )
              )
            ),
            tags$div(
              id = ns("subplotGeometryBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectizeInput(ns("subplot_shape"), label = "Shape", choices =  c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectizeInput(ns("subplot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("subplot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("subplot_dimensions_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$plot_dimension), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectizeInput(ns("subplot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectizeInput(ns("subplot_area_method"), label = "Measurement method", 
                                           choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$plot_area), after = 1)))
                )
              )
            )
          ))
        )
      )
    })
    
    observe({
      if(!is.null(input$subplot_input_control) && input$subplot_input_control != ""){
        shinyjs::show("subplotGeometry")
      } else {
        shinyjs::hide("subplotGeometry")
      }
    })
    
    #-------------------------------------------------------------------------#
    # Observations ####
    abbreviations = c(
      "individualOrganismObservations" = "indOrgObs",
      "aggregateOrganismObservations" = "aggOrgObs",
      "stratumObservations" = "stratumObs",
      "communityObservations" = "commObs",
      "surfaceCoverObservations" = "covObs"
    )
    
    observe({  # Dynamically shows/hides UIs based on checkboxInput
      panel_names =  c("individualOrganismObservations", "aggregateOrganismObservations", "stratumObservations", "communityObservations", "surfaceCoverObservations")
      for(panel_name in panel_names){
        if(panel_name %in% input$observations_input_control){
          shinyjs::show(panel_name)
        } else {
          shinyjs::hide(panel_name)
        }
      }
    })
    
    observe({  # Prevents re-rendering of entire UIs when user_data changes
      if(length(names(user_data)) != 0){
        input_names = paste0(abbreviations, "_data")
        choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
        for(input_name in input_names){
          if(!is.null(input[[input_name]])){
            if(input[[input_name]] %in% file_order()){
              file_selected = input[[input_name]] # save current selection
              choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
              updateSelectizeInput(session, inputId = input_name, selected = file_selected, choices = c(dropdown_empty(), choices))  
            } else {
              choices = names(user_data)[stringr::str_ends(names(user_data), ".xml", negate = T)]
              updateSelectizeInput(session, inputId = input_name, selected = NULL, choices = c(dropdown_empty(), choices))
            }
          }
        }
      }
    })
    
    #------------------------------------#
    ## aggregateOrganismObservations ####
    output$aggOrgObs_ui = renderUI({
      tagList(
        tags$label("Strata definitions"),
        br(),
        tags$p("Are observations structured into strata?", class = "text-info annotation"),
        radioButtons(ns("aggOrgObs_hasStrata"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T),
        uiOutput(ns("aggOrgObs_strata_ui")),
        uiOutput(ns("aggOrgObs_subplot_ui")),
        
        hr(),
        tags$label("Measurement scale *"),
        br(),
        tags$p("Which scale was used to measure the observation?", class = "text-info annotation"),
        selectizeInput(ns("aggOrgObs_measurementScale"), label = NULL, choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$aggOrgObs), after = 1)),
        
        hr(),
        tags$label("Observations *"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A long format table where each aggregate measurement is identified by a unique combination of plot, subplot (optional), date (YYYY-MM-DD format), taxon and stratum (optional)"),
        selectizeInput(ns("aggOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        
        uiOutput(ns("aggOrgObs_mapping_ui"))
      )
    })
    
    output$aggOrgObs_strata_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        tagList(
          tags$p("Which definition was used?", class = "text-info annotation"),
          selectizeInput(ns("aggOrgObs_strataDef"), label = NULL, 
                         choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$strataDef), after = 1))
        )
      }
    })
    
    output$aggOrgObs_stratumName_mapping_ui = renderUI({
      if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
        column(4, selectizeInput(ns("aggOrgObs_stratumName"), label = "Taxon stratum *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
      }
    })
    
    output$aggOrgObs_subplot_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        tagList(
          hr(),
          tags$label("Subplots"),
          br(),
          tags$p("Were observations made at the level of subplots?", class = "text-info annotation"),
          radioButtons(ns("aggOrgObs_hasSubplot"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T)
        )
      }
    })
    
    output$aggOrgObs_subplot_mapping_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes" && isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        column(4, selectizeInput(ns("aggOrgObs_subplot_id"), label = "Subplot ID *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
      }
    })
    
    output$aggOrgObs_mapping_ui = renderUI({
      req(input$aggOrgObs_data)
      tagList(
        fluidRow(
          column(4, selectizeInput(ns("aggOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          uiOutput(ns("aggOrgObs_subplot_mapping_ui")),
          column(4, selectizeInput(ns("aggOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
        ),
        fluidRow(
          column(4, selectizeInput(ns("aggOrgObs_taxonName"), label = "Taxon name *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          uiOutput(ns("aggOrgObs_stratumName_mapping_ui")),
          column(4, selectizeInput(ns("aggOrgObs_taxonMeasurement"), label = "Measurement value *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))    
        )
      )
    })
    
    #------------------------------------#
    ## stratumObservations ####
    output$stratumObs_ui = renderUI({
      tagList(
        tags$label("Strata definitions *"),
        br(),
        tags$p("Which definition was used?", class = "text-info annotation"),
        selectizeInput(ns("stratumObs_strataDef"), label = NULL, 
                       choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$strataDef), after = 1)),
        
        uiOutput(ns("stratumObs_subplot_ui")),
        
        hr(),
        tags$label("Measurement scale *"),
        br(),
        tags$p("Which scale was used to measure the observation?", class = "text-info annotation"),
        selectizeInput(ns("stratumObs_measurementScale"), label = NULL, choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$stratumObs), after = 1)),
        
        hr(),
        tags$label("Observations *"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A long format table where each stratum measurement is identified by a unique combination of plot, subplot (optional), date (YYYY-MM-DD format) and stratum."),
        selectizeInput(ns("stratumObs_data"), label = NULL, choices = c("No files found" = "")),
        
        uiOutput(ns("stratumObs_mapping_ui"))
        
      )
    })
    
    output$stratumObs_subplot_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        tagList(
          hr(),
          tags$label("Subplots"),
          br(),
          tags$p("Were observations made at the level of subplots?", class = "text-info annotation"),
          radioButtons(ns("stratumObs_hasSubplot"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T)
        )
      }
    })
    
    output$stratumObs_subplot_mapping_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes" && isTruthy(input$stratumObs_hasSubplot) && input$stratumObs_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        column(4, selectizeInput(ns("stratumObs_subplot_id"), label = "Subplot ID *", choices = c("Select a column" = "", user_data[[input$stratumObs_data]]$x$rColHeaders)))
      }
    })
    
    output$stratumObs_mapping_ui = renderUI({
      req(input$stratumObs_data)
      tagList(
        fluidRow(
          column(4, selectizeInput(ns("stratumObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$stratumObs_data]]$x$rColHeaders))),
          uiOutput(ns("stratumObs_subplot_mapping_ui")),
          column(4, selectizeInput(ns("stratumObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$stratumObs_data]]$x$rColHeaders)))
        ),
        fluidRow(
          column(4, selectizeInput(ns("stratumObs_stratumName"), label = "Stratum name *", choices = c("Select a column" = "", user_data[[input$stratumObs_data]]$x$rColHeaders))),
          column(4, selectizeInput(ns("stratumObs_stratumMeasurement"), label = "Measurement value *", choices = c("Select a column" = "", user_data[[input$stratumObs_data]]$x$rColHeaders)))    
        )
      )
    })
    
    #------------------------------------#
    ## surfaceCoverObservations ####
    output$covObs_ui = renderUI({
      tagList(
        tags$label("Measurement scale *"),
        br(),
        tags$p("Which scale was used to measure the observation?", class = "text-info annotation"),
        selectizeInput(ns("covObs_measurementScale"), label = NULL, choices = append(list("Select a template" = "", "... define custom method" = "custom_template"), as.list(isolate(methods())$covObs), after = 1)),
        
        uiOutput(ns("covObs_subplot_ui")),
        
        hr(),
        tags$label("Observations *"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A long format table where each surface cover measurement is identified by a unique combination of plot, subplot (optional), date (YYYY-MM-DD format) and surface type"),
        selectizeInput(ns("covObs_data"), label = NULL, choices = c("No files found" = "")),
        
        uiOutput(ns("covObs_mapping_ui"))
      )
    })
    
    output$covObs_subplot_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        tagList(
          hr(),
          tags$label("Subplots"),
          br(),
          tags$p("Were observations made at the level of subplots?", class = "text-info annotation"),
          radioButtons(ns("covObs_hasSubplot"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T)
        )
      }
    })
    
    output$covObs_subplot_mapping_ui = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot  == "yes" && isTruthy(input$covObs_hasSubplot) && input$covObs_hasSubplot  == "yes"){
        req(input$subplot_plot_unique_id, input$subplot_id)
        column(4, selectizeInput(ns("covObs_subplot_id"), label = "Subplot ID *", choices = c("Select a column" = "", user_data[[input$covObs_data]]$x$rColHeaders)))
      }
    })
    
    output$covObs_mapping_ui = renderUI({
      req(input$covObs_data)
      tagList(
        fluidRow(
          column(4, selectizeInput(ns("covObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$covObs_data]]$x$rColHeaders))),
          uiOutput(ns("covObs_subplot_mapping_ui")),
          column(4, selectizeInput(ns("covObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$covObs_data]]$x$rColHeaders)))
        ),
        fluidRow(
          column(4, selectizeInput(ns("covObs_surfaceType"), label = "Surface type *", choices = c("Select a column" = "", user_data[[input$covObs_data]]$x$rColHeaders))),
          column(4, selectizeInput(ns("covObs_surfaceCoverMeasurement"), label = "Measurement value *", choices = c("Select a column" = "", user_data[[input$covObs_data]]$x$rColHeaders)))
        )
      )
    })
    
    
    #------------------------------------#
    ## communityObservations ####
    output$communityObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ## individualOrganismObservations ####
    output$individualOrganismObservations_ui = renderUI({
      tagList(
        tags$p("not implemented")
        # tags$p("Assign a dataset", class = "text-info annotation"),
        # tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A table with one row per subplot (identified by unique combinations of plot and subplot ids) and subplot properties in columns"),
        # selectizeInput(ns("indOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        # uiOutput(ns("indOrgObs_mapping_ui"))
      )
    })
    
    # output$indOrgObs_mapping_ui = renderUI({
    #   req(input$indOrgObs_data)
    #   tagList(
    #     fluidRow(
    #       column(4, selectizeInput(ns("indOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders))),
    #       column(4, selectizeInput(ns("indOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders)))
    #     )
    #   )
    # })
    
    #-------------------------------------------------------------------------#
    # Summary ####
    inputs_complete = reactiveValues()
    
    ## Project ####
    output$summary_project = renderUI({
      inputs_complete$project = check_input_completeness(values = c(input$project_title, input$project_abstract, input$project_citation, input$party_name, input$party_role, input$party_type),
                                                         values_mandatory = 1, values_grouping = list(1,2,3,4:6))
      render_mapping_summary(header = NA, 
                             labels = c("Title *", "Abstract", "Citation", "Party name", "Party role", "Party type"),
                             values = c(input$project_title, input$project_abstract, input$project_citation, input$party_name, input$party_role, input$party_type),
                             inputs_complete$project)
    })
    
    ## Plots ####
    output$summary_plot_id = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot == "yes"){
        if(is.null(input$subplot_plot_unique_id) | is.null(input$subplot_id)){
          inputs_complete$plot_id = F
          render_mapping_summary(header = "Identifier",
                                 labels = c("Plot unique identifier (plot) *", "Plot unique identifier (subplot) *", "Subplot identifier *"),  
                                 values = c(input$plot_unique_id, "", ""),
                                 inputs_complete = inputs_complete$plot_id)  
        } else {
          inputs_complete$plot_id = check_input_completeness(values = c(input$plot_unique_id, input$subplot_plot_unique_id, input$subplot_id),
                                                             values_mandatory = 1:3)
          render_mapping_summary(header = "Identifier",
                                 labels = c("Plot unique identifier (plot) *", "Plot unique identifier (subplot) *", "Subplot identifier *"),  
                                 values = c(input$plot_unique_id, input$subplot_plot_unique_id, input$subplot_id),
                                 inputs_complete = inputs_complete$plot_id)         
        }
        
      } else {
        inputs_complete$plot_id = check_input_completeness(input$plot_unique_id, values_mandatory = 1)
        render_mapping_summary("Identifier", "Unique identifier *", input$plot_unique_id, inputs_complete$plot_id)
      }
    })
    
    output$summary_plot_coordinates = renderUI({
      if("Coordinates" %in% input$plot_input_control){
        inputs_complete$plot_coordinates = check_input_completeness(values = c(input$plot_coordinates_x, input$plot_coordinates_y, templates_lookup()$name[as.numeric(input$plot_location_method)], input$plot_crs))
        render_mapping_summary(header = "Coordinates",
                               labels = c("X-Coordinate", "Y-Coordinate", "Measurement method", "Coordinate Reference System (CRS)"),
                               values = c(input$plot_coordinates_x, input$plot_coordinates_y, templates_lookup()$name[as.numeric(input$plot_location_method)], input$plot_crs),
                               inputs_complete = inputs_complete$plot_coordinates )
      } else {
        inputs_complete$plot_coordinates = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_plot_elevation = renderUI({
      if("Elevation" %in% input$plot_input_control){
        inputs_complete$plot_elevation = check_input_completeness(values = c(input$plot_elevation, templates_lookup()$name[as.numeric(input$plot_elevation_method)]))
        render_mapping_summary(header = "Elevation",
                               labels = c("Plot elevation", "Measurement method"),
                               values = c(input$plot_elevation, templates_lookup()$name[as.numeric(input$plot_elevation_method)]),
                               inputs_complete = inputs_complete$plot_elevation)
      } else {
        inputs_complete$plot_elevation = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_plot_geometry = renderUI({
      if("Geometry" %in% input$plot_input_control){
        inputs_complete$plot_geometry = check_input_completeness(values = c(input$plot_shape, input$plot_length, input$plot_width, templates_lookup()$name[as.numeric(input$plot_dimensions_method)], input$plot_area, templates_lookup()$name[as.numeric(input$plot_area_method)]),
                                                                 values_grouping = list(1, 2:4, 5:6))
        render_mapping_summary(header = "Geometry (plot)",
                               labels = c("Plot shape", "Plot length", "Plot width", "Measurement method (dimensions)", "Plot area", "Measurement method (area)"), 
                               values = c(input$plot_shape, input$plot_length, input$plot_width, templates_lookup()$name[as.numeric(input$plot_dimensions_method)], input$plot_area, templates_lookup()$name[as.numeric(input$plot_area_method)]),
                               inputs_complete = inputs_complete$plot_geometry)
      } else {
        inputs_complete$plot_geometry = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_subplot_geometry = renderUI({
      if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot == "yes" && isTruthy(input$subplot_input_control) && "Geometry" %in% input$subplot_input_control){
        inputs_complete$subplot_geometry = check_input_completeness(values = c(input$subplot_shape, input$subplot_length, input$subplot_width, templates_lookup()$name[as.numeric(input$subplot_dimensions_method)], input$subplot_area, templates_lookup()$name[as.numeric(input$subplot_area_method)]),
                                                                    values_grouping = list(1, 2:4, 5:6))
        render_mapping_summary(header = "Geometry (subplot)",
                               labels = c("Plot shape", "Plot length", "Plot width", "Measurement method (dimensions)", "Plot area", "Measurement method (area)"), 
                               values = c(input$subplot_shape, input$subplot_length, input$subplot_width, templates_lookup()$name[as.numeric(input$subplot_dimensions_method)], input$subplot_area, templates_lookup()$name[as.numeric(input$subplot_area_method)]),
                               inputs_complete = inputs_complete$subplot_geometry)
      } else {
        inputs_complete$subplot_geometry = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_plot_topography = renderUI({
      if("Topography" %in% input$plot_input_control){
        inputs_complete$plot_topography = check_input_completeness(values = c(input$plot_aspect, templates_lookup()$name[as.numeric(input$plot_aspect_method)], input$plot_slope, templates_lookup()$name[as.numeric(input$plot_slope_method)]),
                                                                   values_grouping = list(1:2, 3:4))
        render_mapping_summary(header = "Topography",
                               labels = c("Plot aspect", "Aspect measurement method", "Plot slope", "Slope measurement method"), 
                               values = c(input$plot_aspect, templates_lookup()$name[as.numeric(input$plot_aspect_method)], input$plot_slope, templates_lookup()$name[as.numeric(input$plot_slope_method)]),
                               inputs_complete = inputs_complete$plot_topography)
      } else {
        inputs_complete$plot_topography = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_plot_parent_material = renderUI({
      if("Parent_material" %in% input$plot_input_control){
        inputs_complete$parent_material = check_input_completeness(input$plot_parent_material)
        render_mapping_summary(header = "Parent material", 
                               labels = "Parent material", 
                               values = input$plot_parent_material, 
                               inputs_complete = inputs_complete$parent_material)
      } else {
        inputs_complete$parent_material = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    ## Observations ####
    output$summary_observations = renderUI({
      if(!isTruthy(input$observations_input_control)){
        inputs_complete$observations = F
        return(div(class = "frame frame-danger", h4("No observations selected")))
      } else {
        inputs_complete$observations = T
        tagList(
          uiOutput(ns("summary_aggOrgObs")),
          uiOutput(ns("summary_stratumObs")),
          uiOutput(ns("summary_covObs"))
        )
      }
    })
    
    output$summary_aggOrgObs = renderUI({
      if("aggregateOrganismObservations" %in% input$observations_input_control){
        input_values = list("Plot" = input$aggOrgObs_plot_id, 
                            "Subplot" = input$aggOrgObs_subplot_id, 
                            "Observation date" = input$aggOrgObs_date, 
                            "Taxon name" = input$aggOrgObs_taxonName, 
                            "Stratum definition" = ifelse(isTruthy(input$aggOrgObs_strataDef), templates_lookup()$name[templates_lookup()$template_id == input$aggOrgObs_strataDef], ""), 
                            "Stratum" = input$aggOrgObs_stratumName, 
                            "Measurement scale" = ifelse(is.na(as.numeric(input$aggOrgObs_measurementScale)), input$aggOrgObs_measurementScale, templates_lookup()$name[templates_lookup()$template_id == input$aggOrgObs_measurementScale]), 
                            "Measurement value" = input$aggOrgObs_taxonMeasurement)
        
        if(is.null(input$aggOrgObs_plot_id)){input_values[["Plot"]] = ""}
        if(is.null(input$aggOrgObs_date)){input_values[["Observation date"]]= ""}
        if(is.null(input$aggOrgObs_taxonName)){input_values[["Taxon name"]] = ""}
        if(is.null(input$aggOrgObs_measurementScale)){input_values[["Measurement scale"]] = ""}
        if(is.null(input$aggOrgObs_taxonMeasurement)){input_values[["Measurement value"]] = ""}
        if(isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot == "no"){input_values[["Subplot"]] = NULL}
        if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "no"){input_values[["Stratum"]] = NULL; input_values[["Stratum definition"]] = NULL}
        
        values = Filter(Negate(is.null), input_values) %>% unlist() # removes NULL values
        labels = names(values)
        
        inputs_complete$aggOrgObs = check_input_completeness(values = values, values_mandatory = 1:length(values)) # No groupings, all values mandatory
        render_mapping_summary(header = "AggregateOrganismObservations", labels = labels, values = values, inputs_complete = inputs_complete$aggOrgObs)
      } else {
        inputs_complete$aggOrgObs = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_stratumObs = renderUI({
      if("stratumObservations" %in% input$observations_input_control){
        input_values = list("Plot" = input$stratumObs_plot_id, 
                            "Subplot" = input$stratumObs_subplot_id, 
                            "Observation date" = input$stratumObs_date, 
                            "Stratum definition" = ifelse(isTruthy(input$stratumObs_strataDef), templates_lookup()$name[templates_lookup()$template_id == input$stratumObs_strataDef], ""), 
                            "Stratum name" = input$stratumObs_stratumName, 
                            "Measurement scale" = ifelse(is.na(as.numeric(input$stratumObs_measurementScale)), input$stratumObs_measurementScale, templates_lookup()$name[templates_lookup()$template_id == input$stratumObs_measurementScale]), 
                            "Measurement value" = input$stratumObs_stratumMeasurement)
        
        if(is.null(input$stratumObs_plot_id)){input_values[["Plot"]] = ""}
        if(isTruthy(input$stratumObs_hasSubplot) && input$stratumObs_hasSubplot == "no"){input_values[["Subplot"]] = NULL}
        if(is.null(input$stratumObs_date)){input_values[["Observation date"]]= ""}
        if(is.null(input$stratumObs_measurementScale)){input_values[["Measurement scale"]] = ""}
        if(is.null(input$stratumObs_stratumName)){input_values[["Stratum name"]] = ""}
        if(is.null(input$stratumObs_stratumMeasurement)){input_values[["Measurement value"]] = ""}
        
        values = Filter(Negate(is.null), input_values) %>% unlist() # removes NULL values
        labels = names(values)
        
        inputs_complete$stratumObs = check_input_completeness(values = values, values_mandatory = 1:length(values))  # No groupings, all values mandatory
        render_mapping_summary(header = "stratumObservations", labels = labels, values = values, inputs_complete = inputs_complete$stratumObs)
      } else {
        inputs_complete$stratumObs = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    output$summary_covObs = renderUI({
      if("surfaceCoverObservations" %in% input$observations_input_control){
        input_values = list("Plot" = input$covObs_plot_id, 
                            "Subplot" = input$covObs_subplot_id, 
                            "Observation date" = input$covObs_date, 
                            "Surface type" = input$covObs_surfaceType, 
                            "Measurement scale" = ifelse(is.na(as.numeric(input$covObs_measurementScale)), input$covObs_measurementScale, templates_lookup()$name[templates_lookup()$template_id == input$covObs_measurementScale]), 
                            "Measurement value" = input$covObs_surfaceCoverMeasurement)
        
        if(is.null(input$covObs_plot_id)){input_values[["Plot"]] = ""}
        if(isTruthy(input$covObs_hasSubplot) && input$covObs_hasSubplot == "no"){input_values[["Subplot"]] = NULL}
        if(is.null(input$covObs_date)){input_values[["Observation date"]]= ""}
        if(is.null(input$covObs_measurementScale)){input_values[["Measurement scale"]] = ""}
        if(is.null(input$covObs_surfaceType)){input_values[["Surface type"]] = ""}
        if(is.null(input$covObs_surfaceCoverMeasurement)){input_values[["Measurement value"]] = ""}
        
        values = Filter(Negate(is.null), input_values) %>% unlist() # removes NULL values
        labels = names(values)
        
        inputs_complete$covObs = check_input_completeness(values = values, values_mandatory = 1:length(values))  # No groupings, all values mandatory
        render_mapping_summary(header = "surfaceCoverObservations", labels = labels, values = values, inputs_complete = inputs_complete$covObs)
      } else {
        inputs_complete$covObs = T  # Set completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    
    #-------------------------------------------------------------------------#
    # Build Nodes ####
    observeEvent(
      eventExpr = input$import, 
      handlerExpr = {
        # Build Modal UI elements
        if(all(sapply(reactiveValuesToList(inputs_complete), isTRUE))){
          modal_content = div(class = "text-center text-info", icon("check"), tags$p("This will add all mappings to your VegX document."))
          modal_footer = tagList(
            tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")), 
                      actionButton(ns("confirm_import"), class = "pull-right btn-success", "Proceed", icon("check")))
          )
        } else {
          modal_content = div(class = "text-center text-danger", icon("exclamation"), tags$p("Submission incomplete. Please review your entries."))
          modal_footer = tagList(tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")), 
                                           shinyjs::disabled(actionButton(ns("confirm_import"), class = "pull-right btn-success", "Proceed", icon("check"))))
          )
        }
        
        # Show modal dialog
        showModal(
          modalDialog(tags$h3("Import data"),
                      hr(),
                      modal_content,
                      size = "l",
                      footer = modal_footer
          )
        )
      }
    )
    
    observeEvent(
      eventExpr = input$dismiss_modal, 
      handlerExpr = {
        removeModal()
      }
    )
    
    observeEvent(
      eventExpr = input$confirm_import,
      handlerExpr = {
        tryCatch({
          # Remove attributes and child nodes from vegx_doc
          vegx_doc %>% xml_find_all("//vegX") %>% xml_children() %>% xml_remove()
          
          # Initialize an ID factory with one id_generator per ID name as defined by id_lookup (see /data-raw)
          id_factory = sapply(unique(id_lookup), function(x){
            id_generator()
          }, simplify = F)
          
          #-------------------------------------------------------------------------#
          withProgress(
            message = "Importing data",
            expr = {
              # Preparations 
              shinyjs::disable("confirm_import")
              shinyjs::disable("dismiss_modal")
              nodes = list()  
              
              ## Project ####
              setProgress(value = 0.05, "Projects")
              if(isTruthy(input$party_name) & isTruthy(input$party_type)){
                parties_df = data.frame(input$party_name, check.names = F)
                names(parties_df) = paste0("party > choice > ", tolower(input$party_type), "Name")
                nodes$parties = new_vegx_nodes(parties_df, vegx_schema, id_factory)
              }
              
              if(isTruthy(input$project_citation)){
                citations_df = data.frame("literatureCitation > citationString" = input$project_citation, check.names = F)
                nodes$literatureCitations = new_vegx_nodes(citations_df, vegx_schema, id_factory)
              }
              
              project_df = data.frame("project > title" = input$project_title, check.names = F)
              if(isTruthy(input$project_abstract)){
                project_df[["project > abstract"]] = input$project_abstract
              }
              if(isTruthy(input$party_name) & isTruthy(input$party_role)){
                project_df[["project > personnel > role"]] = input$party_role
              }
              if(length(nodes$parties) > 0 && isTruthy(input$party_role)){
                project_df[["project > personnel > partyID"]] = xml_attr(nodes$parties[[1]], "id")
                project_df[["project > personnel > role"]] = input$party_role
              }
              if(length(nodes$literatureCitations) > 0){
                project_df[["project > documentCitationID"]] = xml_attr(nodes$literatureCitations[[1]], "id")
              }
              
              nodes$projects = new_vegx_nodes(project_df, vegx_schema, id_factory)
              
              #-------------------------------------------------------------------------#
              ## Plots ####
              if(!is.null(input$plot_unique_id)){ # Check if UI has been rendered already
                # Fetch user data assigned to plots
                setProgress(value = 0.1, "Plots")
                plots_upload = user_data[[input$plot_data]]
                plots_df_upload = jsonlite::fromJSON(plots_upload$x$data)
                colnames(plots_df_upload) = plots_upload$x$rColHeaders
                plots_df_upload = data.frame(plots_df_upload, check.names = F)
                plots_df_upload[plots_df_upload==""] = NA  
                
                # Check identifiers
                plot_ids = plots_df_upload[[input$plot_unique_id]]
                if("" %in% plot_ids){
                  stop("Error while importing plots: Plot IDs may not contain empty values")
                }
                if(length(plot_ids) != length(unique(plot_ids))){
                  stop("Error while importing plots: Plot IDs must be unique")
                }
                
                # Build mappings table
                plots_df = data.frame(
                  "plot > plotName" = plot_ids,
                  "plot > plotUniqueIdentifier" = plot_ids,
                  check.names = F
                )
                
                if("Coordinates" %in% input$plot_input_control){
                  if(isTruthy(input$plot_coordinates_x) && isTruthy(input$plot_coordinates_y) && isTruthy(input$plot_location_method) && isTruthy(input$plot_crs)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_location_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > location > horizontalCoordinates > coordinates > valueX"]] = plots_df_upload[[input$plot_coordinates_x]]
                    plots_df[["plot > location > horizontalCoordinates > coordinates > valueY"]] = plots_df_upload[[input$plot_coordinates_y]]
                    plots_df[["plot > location > horizontalCoordinates > coordinates > spatialReference"]] = input$plot_crs
                    plots_df[["plot > location > horizontalCoordinates > coordinates > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Elevation" %in% input$plot_input_control){
                  if(isTruthy(input$plot_elevation) && isTruthy(input$plot_elevation_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_elevation_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > location > verticalCoordinates > elevation > value"]] = plots_df_upload[[input$plot_elevation]]
                    plots_df[["plot > location > verticalCoordinates > elevation > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Geometry" %in% input$plot_input_control){
                  if(isTruthy(input$plot_shape)){
                    plots_df[["plot > geometry > shape"]] = plots_df_upload[[input$plot_shape]]
                  }
                  if(isTruthy(input$plot_length) && isTruthy(input$plot_width) && isTruthy(input$plot_dimesion_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_dimensions_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > geometry > length > value"]] = plots_df_upload[[input$plot_length]]
                    plots_df[["plot > geometry > length > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    plots_df[["plot > geometry > width > value"]] = plots_df_upload[[input$plot_width]]
                    plots_df[["plot > geometry > width > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                  if(isTruthy(input$plot_area) && isTruthy(input$plot_area_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_area_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > geometry > area > value"]] = plots_df_upload[[input$plot_area]]
                    plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Topography" %in% input$plot_input_control){
                  if(isTruthy(input$plot_aspect) && isTruthy(input$plot_aspect_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_aspect_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > topography > aspect > value"]] = plots_df_upload[[input$plot_aspect]]
                    plots_df[["plot > topography > aspect > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                  if(isTruthy(input$plot_slope) && isTruthy(input$plot_slope_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_slope_method) %>% templates_to_nodes(vegx_schema, id_factory)
                    
                    plots_df[["plot > topography > slope > value"]] = plots_df_upload[[input$plot_slope]]
                    plots_df[["plot > topography > slope > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]], "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Parent material" %in% input$plot_input_control){
                  if(isTruthy(input$plot_parent_material)){
                    plots_df[["plot > parentMaterial > value"]] = plots_df_upload[[input$plot_parent_material]]
                  }
                }
                
                # Build plot nodes 
                plot_nodes = new_vegx_nodes(plots_df, vegx_schema, id_factory)
                nodes$plots = append(nodes$plots, plot_nodes)
                
                plots_lookup = data.frame(
                  plotID = sapply(nodes$plots, function(x){xml_attr(x, "id")}), # The internal id used by vegXshiny
                  plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x, "//plotUniqueIdentifier"))}), # the mapped unique identifier in the data
                  check.names = F
                )
                
                #------------------------------------#
                ## Subplots ####
                if(!is.null(input$plot_hasSubplot) && input$plot_hasSubplot == "yes" && isTruthy(input$subplot_data)){
                  # Fetch user data assigned plots
                  subplots_upload = user_data[[input$subplot_data]]
                  subplots_df_upload = jsonlite::fromJSON(subplots_upload$x$data)
                  colnames(subplots_df_upload) = subplots_upload$x$rColHeaders
                  subplots_df_upload = data.frame(subplots_df_upload, check.names = F)
                  subplots_df_upload[subplots_df_upload==""] = NA  
                  
                  # Build mappings table
                  plot_ids = subplots_df_upload[[input$subplot_plot_unique_id]]
                  subplot_ids = subplots_df_upload[[input$subplot_id]]
                  plotUniqueIdentifiers = paste(subplots_df_upload[[input$subplot_plot_unique_id]], subplots_df_upload[[input$subplot_id]], sep = "-")
                  
                  if(length(setdiff(plot_ids, plots_lookup$plotUniqueIdentifier)) != 0){
                    stop("Subplot data contain unknown plot IDs")
                  } else if("" %in% subplot_ids){
                    stop("Subplot IDs may not contain empty values")
                  } else if(length(plotUniqueIdentifiers) != length(unique(plotUniqueIdentifiers))){
                    stop("Combination of PlotID and SubPlotID must be unique")
                  }
                  
                  subplots_df = data.frame(
                    "plot > plotName" = subplots_df_upload[[input$subplot_id]],
                    "plot > plotUniqueIdentifier" = plotUniqueIdentifiers,
                    "plot > relatedPlot > relatedPlotID" = subplots_df_upload %>% 
                      dplyr::select(plotUniqueIdentifier = input$subplot_plot_unique_id) %>% 
                      left_join(plots_lookup, by = "plotUniqueIdentifier") %>% 
                      pull(plotID),          
                    "plot > relatedPlot > plotRelationship" = "subplot",
                    check.names = F
                  ) %>% filter(stats::complete.cases(.))
                  
                  if("Geometry" %in% input$subplot_input_control){
                    if(isTruthy(input$subplot_shape)){
                      subplots_df[["plot > geometry > shape"]] = subplots_df_upload[[input$subplot_shape]]
                    }
                    if(isTruthy(input$subplot_length) && isTruthy(input$subplot_width) && isTruthy(input$subplot_dimesion_method)){
                      method_nodes = templates() %>% dplyr::filter(template_id == input$subplot_dimensions_method) %>% templates_to_nodes(vegx_schema, id_factory)
                      
                      subplots_df[["plot > geometry > length > value"]] = subplots_df_upload[[input$subplot_length]]
                      subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                      subplots_df[["plot > geometry > width > value"]] = subplots_df_upload[[input$subplot_width]]
                      subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                      
                      nodes$methods = append(nodes$methods, method_nodes$methods)
                      nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                    }
                    if(isTruthy(input$subplot_area) && isTruthy(input$subplot_area_method)){
                      method_template = templates() %>% dplyr::filter(template_id ==  input$subplot_area_method)
                      
                      # Check if method exists already 
                      method_name = method_template[method_template$node_path == "method > name", "node_value"] 
                      methods_lookup = data.frame(
                        methodID = sapply(nodes$methods, function(x){xml2::xml_attr(x, "id")}), # The internal id used by vegXshiny
                        methodName = sapply(nodes$methods, function(x){xml2::xml_text(xml2::xml_find_all(x, "..//name"))}), # the mapped unique identifier in the data
                        check.names = F
                      )
                      
                      if(method_name %in% methods_lookup$methodName){  # If yes, use existing ID
                        attributes_method_links = sapply(nodes$attributes, function(x){xml2::xml_text(xml2::xml_find_all(x, "..//methodID"))})
                        attribute_node = nodes$attributes[[which(attributes_method_links == methods_lookup$methodID[methods_lookup$methodName == method_name])]]
                        attribute_id = xml2::xml_attr(attribute_node, "id")
                      } else {                                         # If no, create new nodes
                        method_nodes = templates_to_nodes(method_template, vegx_schema, id_factory)
                        nodes$methods = append(nodes$methods, method_nodes$methods)
                        nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                        attribute_id = xml2::xml_attr(method_nodes$attributes[[1]], "id")
                      }
                      
                      subplots_df[["plot > geometry > area > value"]] = subplots_df_upload[[input$subplot_area]]
                      subplots_df[["plot > geometry > area > attributeID"]] = attribute_id
                    }
                  }
                  
                  # Build subplot nodes 
                  subplot_nodes = new_vegx_nodes(subplots_df, vegx_schema, id_factory)
                  nodes$plots = append(nodes$plots, subplot_nodes)
                  
                  # Update plot lookup
                  plots_lookup = data.frame(
                    plotID = sapply(nodes$plots, function(x){xml_attr(x, "id")}), # The internal id used by vegXshiny
                    plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x, "//plotUniqueIdentifier"))}), # the mapped unique identifier in the data
                    check.names = F
                  )
                }
              }
              
              #-------------------------------------------------------------------------#
              ## Observations ####
              # The central element in VegX is the plotObservation, which is referenced by all other observation types. 
              # Additionally, a number of other elements such as methods, organismNames, strata etc. may be shared by different observationTypes.
              # This provides a logical order for building up the VegX document when importing observations: First, build plotObservations from
              # all unique combinations of plot, subplot (if provided) and date across all observations. Second, systematically process potentially shared elements and 
              # create new nodes from the mappings for different observationTypes. Finally, resolve the mapped values to their corresponding 
              # node IDs and create the actual observation elements. 
              #
              # The workflow below follows this rationale.
              
              if(!is.null(input$observations_input_control) && all(input$observations_input_control != "")){
                setProgress(value = 0.2, "Plot observations")
                # Fetch user data assigned in observation mappings
                data_upload = sapply(input$observations_input_control, function(obs_category){
                  input_value = input[[paste0(abbreviations[obs_category], "_data")]]
                  file_name = str_split(input_value, "\\$", simplify = T)[1]
                  upload = user_data[[file_name]]
                  df_upload = jsonlite::fromJSON(upload$x$data)
                  colnames(df_upload) = upload$x$rColHeaders
                  return(data.frame(df_upload, check.names = F))
                }, simplify = FALSE, USE.NAMES = TRUE)
                
                #------------------------------------#
                # Build plotObservations
                # 1. Get unique plot-date combinations across observations, to avoid creation of duplicate plotObservations
                plotObs_df_list = lapply(input$observations_input_control, function(obs_category){
                  df_upload = data_upload[[obs_category]] 
                  
                  # Check date formats
                  date_input = input[[paste0(abbreviations[obs_category], "_date")]]
                  date_vec = df_upload %>% pull(date_input) 
                  tryCatch(lubridate::ymd(date_vec), warning = function(w){
                    stop(paste0("Could not parse date field for ", obs_category, ". Please provide all dates in YYYY-MM-DD format."))
                  })
                  
                  # Get Identifiers
                  plotUniqueIdentifier = df_upload[,input[[paste0(abbreviations[obs_category], "_plot_id")]]]
                  
                  inputName_subplot_id = paste0(abbreviations[obs_category], "_subplot_id")
                  inputName_hasSubplot = paste0(abbreviations[obs_category], "_hasSubplot")
                  if(isTruthy(input[[inputName_subplot_id]]) && input[[inputName_hasSubplot ]] == "yes"){
                    plotUniqueIdentifier = paste(plotUniqueIdentifier, df_upload[,input[[inputName_subplot_id]]], sep = "-")
                  }
                  date = df_upload[,input[[paste0(abbreviations[obs_category], "_date")]]]
                  
                  return(data.frame("plotUniqueIdentifier" = plotUniqueIdentifier, 
                                    "plotObservation > obsStartDate" = date, 
                                    check.names = F))
                }) 
                
                plotObs_df = plotObs_df_list %>% 
                  bind_rows() %>% 
                  distinct() %>% 
                  filter(stats::complete.cases(.)) %>% 
                  mutate("plotObservation > projectID" = xml2::xml_attr(nodes$projects[[1]], attr = "id"))
                
                # 2. Check if plots have ids already
                plots_unmatched = setdiff(plotObs_df$plotUniqueIdentifier, 
                                          sapply(nodes$plots, function(x){xml_text(xml_find_first(x, "//plotUniqueIdentifier"))}))
                
                if(length(plots_unmatched) > 0){
                  stop("Plot identifiers do not match between plot data and observation data")
                  plots_df_addendum =  data.frame("plot > plotName" = plots_unmatched, "plot > plotUniqueIdentifier" = plots_unmatched, check.names = F)
                  plot_nodes_addendum = new_vegx_nodes(plots_df_addendum, vegx_schema, id_factory)
                  nodes$plots = append(nodes$plots, plot_nodes_addendum)
                  shiny::showNotification(type = "warning", paste0("Observation data referenced unknown plots. Added ", length(plots_unmatched),
                                                                   " new plot nodes for the following plots: ", plots_unmatched))
                }
                
                # 3. Replace mapped plot identifiers with internal ids
                plotObs_df = plotObs_df %>% 
                  inner_join(plots_lookup, by = "plotUniqueIdentifier") %>% 
                  mutate("plotObservation > plotID" = plotID) %>% 
                  dplyr::select(-plotUniqueIdentifier, -plotID)
                
                # 4. Create nodes
                plotObs_nodes = new_vegx_nodes(plotObs_df, vegx_schema, id_factory)
                nodes$plotObservations = append(nodes$plotObservations, plotObs_nodes)  
                
                # 5. Build lookup table
                plotObs_lookup = lapply(plotObs_nodes, function(x){
                  data.frame(plotObservationID = xml2::xml_attr(x, "id"),
                             plotID = xml2::xml_text(xml2::xml_child(x, search = "plotID")),
                             obs_date = lubridate::ymd(xml2::xml_text(xml2::xml_child(x, search = "obsStartDate"))),
                             check.names = F)}) %>% 
                  bind_rows() %>% 
                  left_join(plots_lookup, by = "plotID")
                
                #------------------------------------#
                # Build organismNames and organismIdentities
                # 1. Fetch data
                setProgress(value = 0.3, "Organisms")
                orgNames = c() # organism names may come from indOrgObs or aggOrgObs
                if("individualOrganismObservations" %in% input$observations_input_control){
                  orgNames = c(orgNames, data_upload[["individualOrganismObservations"]][,input$indOrgObs_taxonName])
                }
                if("aggregateOrganismObservations" %in% input$observations_input_control){
                  orgNames = c(orgNames, data_upload[["aggregateOrganismObservations"]][, input$aggOrgObs_taxonName])
                }  
                
                if(length(orgNames) > 0){ 
                  # 2. Build Nodes
                  orgNames_df = data.frame("organismName" = unique(orgNames), check.names = F)
                  orgNames_nodes = new_vegx_nodes(orgNames_df, vegx_schema, id_factory)
                  sapply(orgNames_nodes, function(node){xml_set_attr(node, "taxonName", "true")})
                  
                  orgIdentities_df = data.frame("organismIdentity > originalOrganismNameID" = sapply(orgNames_nodes, function(x){xml2::xml_attr(x, attr = "id")}), check.names = F)
                  orgIdentities_nodes = new_vegx_nodes(orgIdentities_df, vegx_schema, id_factory)
                  
                  nodes$organismNames = orgNames_nodes
                  nodes$organismIdentities = orgIdentities_nodes
                  
                  # 3. Build lookup table
                  orgIdentities_lookup = lapply(orgIdentities_nodes, function(x){
                    data.frame(organismIdentityID = xml2::xml_attr(x, "id"),
                               originalOrganismNameID = xml2::xml_text(xml2::xml_child(x, search = "originalOrganismNameID")))}) %>% 
                    bind_rows()
                  
                  orgNames_lookup = bind_cols(orgNames_df, orgIdentities_df) %>% 
                    setNames(c("organismName", "originalOrganismNameID")) 
                  
                  organisms_lookup = left_join(orgIdentities_lookup, orgNames_lookup, by = "originalOrganismNameID")
                }
                
                #------------------------------------#
                # Build Strata, if available
                setProgress(value = 0.4, "Strata")
                strataDef_template_ids = c()
                strata_observed = c() # strata may come from aggOrgObs or stratumObs
                
                if("aggregateOrganismObservations" %in% input$observations_input_control && isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
                  strataDef_template_ids = c(strataDef_template_ids, input$aggOrgObs_strataDef)
                  strata_observed = c(strata_observed, unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_stratumName]))
                }
                if("stratumObservations" %in% input$observations_input_control && isTruthy(input$stratumObs_strataDef)){
                  strataDef_template_ids = c(strataDef_template_ids, input$stratumObs_strataDef)
                  strata_observed = c(strata_observed, unique(data_upload[["stratumObservations"]][,input$stratumObs_stratumName]))
                }
                
                if(length(strataDef_template_ids) > 0){ 
                  strataDef_templates = templates() %>% dplyr::filter(template_id %in% strataDef_template_ids)
                  
                  # Check if observations contain undefined strata, add unmatched strata to templates
                  strata_template = strataDef_templates %>% dplyr::filter(node_path == "stratum > stratumName") %>% pull(node_value)
                  strata_unmatched = setdiff(strata_observed, strata_template)
                  
                  if(length(strata_unmatched) != 0){ # Add new values
                    strataDef_templates = lapply(as.numeric(unique(strataDef_template_ids)), function(template_id){
                      template = templates() %>% dplyr::filter(template_id == !!template_id)
                      template_addendum = data.frame(template_id = template_id, 
                                                     main_element = "strata", 
                                                     node_path = "stratum > stratumName", 
                                                     node_value = strata_unmatched,
                                                     group_value = strata_unmatched) %>% 
                        group_by(group_value) %>% 
                        group_modify(~add_row(.x, template_id = !!template_id, main_element = "strata", node_path = "stratum > methodID", node_value = "1")) %>%
                        mutate(node_id = cur_group_id()+max(template$node_id)) %>% 
                        ungroup() %>% 
                        dplyr::select(template_id, node_id, main_element, node_path, node_value)
                      
                      template_extended = bind_rows(template, template_addendum)
                      return(template_extended)
                    }) %>% bind_rows()
                  }
                  
                  strataDef_nodes = templates_to_nodes(strataDef_templates, vegx_schema, id_factory)
                  nodes$strata = append(nodes$strata, strataDef_nodes$strata)
                  nodes$methods = append(nodes$methods, strataDef_nodes$methods)
                  nodes$attributes = append(nodes$attributes, strataDef_nodes$attributes)
                  
                  strata_lookup = lapply(nodes$strata, function(x){
                    data.frame(methodID = xml2::xml_text(xml2::xml_child(x, search = "methodID")),
                               stratumID = xml2::xml_attr(x, "id"),
                               stratumName = xml2::xml_text(xml2::xml_child(x, search = "stratumName")))}) %>% 
                    bind_rows()
                }
                
                #------------------------------------#
                # Build Observations
                progress_increment = 0.4 / (length(input$observations_input_control) + 1)
                
                # -----------------------------------#
                # AggregatOrganismObservations
                incProgress(progress_increment, "AggregateOrganismObservations")
                if("aggregateOrganismObservations" %in% input$observations_input_control){
                  if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
                    # Build mapping table
                    plotUniqueIdentifier = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_plot_id]
                    if(isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot == "yes"){
                      plotUniqueIdentifier = paste(plotUniqueIdentifier, data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_subplot_id], sep = "-")
                    }
                    
                    aggOrgObs_stratumObs_df = data.frame(
                      plotUniqueIdentifier = plotUniqueIdentifier,
                      obs_date = lubridate::ymd(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_date]),
                      stratumName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_stratumName]
                    ) %>%  
                      left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                      left_join(strata_lookup, by = "stratumName") %>% 
                      dplyr::select("stratumObservation > plotObservationID" = plotObservationID, 
                                    "stratumObservation > stratumID" = stratumID) %>% 
                      distinct() %>% 
                      arrange("stratumObservation > plotObservationID", "stratumObservation > stratumID")
                    
                    # Create nodes
                    aggOrgObs_stratumObs_nodes = new_vegx_nodes(aggOrgObs_stratumObs_df, vegx_schema, id_factory)
                    nodes$stratumObservations = append(nodes$stratumObservations, aggOrgObs_stratumObs_nodes)  
                    
                    # Build lookup table
                    stratumObs_lookup = lapply(nodes$stratumObservations, function(x){
                      data.frame(stratumObservationID = xml2::xml_attr(x, "id"),
                                 plotObservationID = xml2::xml_text(xml2::xml_child(x, search = "plotObservationID")),
                                 stratumID = xml2::xml_text(xml2::xml_child(x, search = "stratumID")))}) %>% 
                      bind_rows()
                  }
                  
                  #------------------#
                  aggOrgObs_measurementScale_template = templates() %>% dplyr::filter(template_id == input$aggOrgObs_measurementScale)
                  method_is_quantitative = aggOrgObs_measurementScale_template %>% dplyr::filter(main_element == "attributes") %>% pull(node_path) %>% stringr::str_detect("quantitative") %>% all()
                  
                  if(!method_is_quantitative){
                    node_path =  aggOrgObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_path) %>% unique()
                    codes_template = aggOrgObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_value)
                    codes_observed = sort(unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement]))
                    codes_unmatched = setdiff(codes_observed, codes_template)
                    
                    if(length(codes_unmatched) != 0){   # Check if observations contain undefined measurement categories
                      attributes_addendum = data.frame(template_id = aggOrgObs_measurementScale_template$template_id[1], 
                                                       main_element = "attributes", 
                                                       node_path = "attribute > choice > ordinal > code", 
                                                       node_value = codes_unmatched,
                                                       group_value = codes_unmatched) %>% 
                        group_by(group_value) %>% 
                        group_modify(~add_row(.x, template_id = aggOrgObs_measurementScale_template$template_id[1], main_element = "attributes", node_path = "attribute > choice > ordinal > methodID", node_value = "1")) %>%
                        mutate(node_id = cur_group_id()+max( aggOrgObs_measurementScale_template$node_id)) %>% 
                        ungroup() %>% 
                        dplyr::select(template_id, node_id, main_element, node_path, node_value)
                      
                      aggOrgObs_measurementScale_template = bind_rows(aggOrgObs_measurementScale_template, attributes_addendum)
                    }
                  }
                  
                  # 2. Build Nodes
                  aggOrgObs_measurementScale_nodes = templates_to_nodes(aggOrgObs_measurementScale_template, vegx_schema, id_factory)
                  nodes$methods = append(nodes$methods, aggOrgObs_measurementScale_nodes$methods)
                  nodes$attributes = append(nodes$attributes, aggOrgObs_measurementScale_nodes$attributes)
                  
                  # 3. Build lookup
                  if(method_is_quantitative){
                    measurementScale_lookup = data.frame(attributeID = xml2::xml_attr(aggOrgObs_measurementScale_nodes$attributes[[1]], "id"),
                                                         taxon_measurement = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement]) %>% 
                      dplyr::distinct()
                  } else {
                    measurementScale_lookup = lapply(aggOrgObs_measurementScale_nodes$attributes, function(x){
                      data.frame(attributeID = xml2::xml_attr(x, "id"),
                                 taxon_measurement = xml2::xml_text(xml2::xml_find_first(x, "..//code")))}) %>% 
                      bind_rows()
                  }
                  
                  #------------------#
                  plotUniqueIdentifier = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_plot_id]
                  if(isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot == "yes"){
                    plotUniqueIdentifier = paste(plotUniqueIdentifier, data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_subplot_id], sep = "-")
                  }
                  
                  aggOrgObs_mappings = data.frame(
                    plotUniqueIdentifier = plotUniqueIdentifier,
                    obs_date = lubridate::ymd(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_date]),
                    organismName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonName],
                    taxon_measurement = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement]
                  )
                  
                  if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
                    aggOrgObs_mappings$stratumName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_stratumName]
                    
                    aggOrgObs_df = aggOrgObs_mappings %>% 
                      left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                      left_join(organisms_lookup, by = "organismName") %>% 
                      left_join(measurementScale_lookup, by = "taxon_measurement") %>%
                      left_join(strata_lookup, by = "stratumName") %>%                       # TODO: Check for correct join (2 strataDefs with overlapping category names)
                      left_join(stratumObs_lookup, by = c("stratumID", "plotObservationID")) %>% 
                      dplyr::select("aggregateOrganismObservation > plotObservationID" = plotObservationID, 
                                    "aggregateOrganismObservation > organismIdentityID" = organismIdentityID, 
                                    "aggregateOrganismObservation > aggregateOrganismMeasurement > value" = taxon_measurement, 
                                    "aggregateOrganismObservation > aggregateOrganismMeasurement > attributeID" = attributeID,
                                    "aggregateOrganismObservation > stratumObservationID" = stratumObservationID)
                  } else {
                    aggOrgObs_df = aggOrgObs_mappings %>% 
                      left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                      left_join(organisms_lookup, by = "organismName") %>% 
                      left_join(measurementScale_lookup, by = "taxon_measurement") %>% 
                      dplyr::select("aggregateOrganismObservation > plotObservationID" = plotObservationID, 
                                    "aggregateOrganismObservation > organismIdentityID" = organismIdentityID, 
                                    "aggregateOrganismObservation > aggregateOrganismMeasurement > value" = taxon_measurement, 
                                    "aggregateOrganismObservation > aggregateOrganismMeasurement > attributeID" = attributeID)
                  }
                  
                  aggOrgObs_nodes = new_vegx_nodes(aggOrgObs_df, vegx_schema, id_factory)
                  nodes$aggregateOrganismObservations = aggOrgObs_nodes  
                }
                
                # -----------------------------------#
                if("stratumObservations" %in% input$observations_input_control){
                  incProgress(progress_increment, "StratumObservations")
                  stratumObs_measurementScale_template = templates() %>% dplyr::filter(template_id == input$stratumObs_measurementScale)
                  
                  # Check if method exists already 
                  method_name = stratumObs_measurementScale_template[stratumObs_measurementScale_template$node_path == "method > name", "node_value"]
                  method_is_quantitative = stratumObs_measurementScale_template %>% dplyr::filter(main_element == "attributes") %>% pull(node_path) %>% stringr::str_detect("quantitative") %>% all()
                  methods_lookup = data.frame(
                    methodID = sapply(nodes$methods, function(x){xml2::xml_attr(x, "id")}), # The internal id used by vegXshiny
                    methodName = sapply(nodes$methods, function(x){xml2::xml_text(xml2::xml_find_all(x, "..//name"))}) # the mapped unique identifier in the data
                  )
                  
                  if(method_name %in% methods_lookup$methodName){   # Method exists already --> build lookup from nodes
                    method_id = methods_lookup$methodID[which(methods_lookup$methodName == method_name)]
                    attribute_nodes = nodes$attributes[sapply(nodes$attributes, function(x){xml2::xml_attr(x, "id") == method_id})]
                    
                    if(method_is_quantitative){
                      measurementScale_lookup = data.frame(attributeID = xml2::xml_attr(attribute_nodes[[1]], "id"),
                                                           stratumMeasurement = data_upload[["stratumObservations"]][,input$stratumObs_stratumMeasurement]) %>% 
                        dplyr::distinct()
                    } else {
                      measurementScale_lookup = lapply(attribute_nodes, function(x){
                        data.frame(attributeID = xml2::xml_attr(x, "id"),
                                   stratumMeasurement = xml2::xml_text(xml2::xml_find_first(x, "..//code")))}) %>% 
                        bind_rows()
                    }
                  } else {       # Method does not exist --> build from scratch (if it's not quantitative)
                    if(!method_is_quantitative){
                      node_path = stratumObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_path) %>% unique()
                      codes_template = stratumObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_value)
                      codes_observed = sort(unique(data_upload[["stratumObservations"]][,input$stratumObs_stratumMeasurement]))
                      codes_unmatched = setdiff(codes_observed, codes_template)
                      
                      if(length(codes_unmatched) != 0){    # Check if observations contain undefined measurement categories
                        attributes_addendum = data.frame(template_id = stratumObs_measurementScale_template$template_id[1], 
                                                         main_element = "attributes", 
                                                         node_path = !!node_path, 
                                                         node_value = codes_unmatched,
                                                         group_value = codes_unmatched) %>% 
                          group_by(group_value) %>% 
                          group_modify(~add_row(.x, template_id = stratumObs_measurementScale_template$template_id[1], main_element = "attributes", node_path = node_path, node_value = "1")) %>%
                          mutate(node_id = cur_group_id()+max(stratumObs_measurementScale_template$node_id)) %>% 
                          ungroup() %>% 
                          dplyr::select(template_id, node_id, main_element, node_path, node_value)
                      }
                      stratumObs_measurementScale_template = bind_rows(stratumObs_measurementScale_template, attributes_addendum)
                    }
                    
                    # 2. Build Nodes
                    stratumObs_measurementScale_nodes = templates_to_nodes(stratumObs_measurementScale_template, vegx_schema, id_factory)
                    nodes$methods = append(nodes$methods, stratumObs_measurementScale_nodes$methods)
                    nodes$attributes = append(nodes$attributes, stratumObs_measurementScale_nodes$attributes)
                    
                    # 3. Build lookup (if qualitative scale was used)
                    if(method_is_quantitative){
                      measurementScale_lookup = data.frame(attributeID = xml2::xml_attr(stratumObs_measurementScale_nodes$attributes[[1]], "id"),
                                                           stratumMeasurement = data_upload[["stratumObservations"]][,input$stratumObs_stratumMeasurement]) %>% 
                        dplyr::distinct()
                    } else {
                      measurementScale_lookup = lapply(stratumObs_measurementScale_nodes$attributes, function(x){
                        data.frame(attributeID = xml2::xml_attr(x, "id"),
                                   stratumMeasurement = xml2::xml_text(xml2::xml_find_first(x, "..//code")))}) %>% 
                        bind_rows()
                    }
                  }
                  
                  # Build mapping table
                  plotUniqueIdentifier = data_upload[["stratumObservations"]][,input$stratumObs_plot_id]
                  if(isTruthy(input$stratumObs_hasSubplot) && input$stratumObs_hasSubplot == "yes"){
                    plotUniqueIdentifier = paste(plotUniqueIdentifier, data_upload[["stratumObservations"]][,input$stratumObs_subplot_id], sep = "-")
                  }
                  
                  stratumObs_df = data.frame(plotUniqueIdentifier = plotUniqueIdentifier,
                                             obs_date = lubridate::ymd(data_upload[["stratumObservations"]][,input$stratumObs_date]),
                                             stratumName = data_upload[["stratumObservations"]][,input$stratumObs_stratumName],
                                             stratumMeasurement = data_upload[["stratumObservations"]][,input$stratumObs_stratumMeasurement]) %>%  
                    left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                    left_join(strata_lookup, by = "stratumName") %>%                 # TODO: Check for correct join (2 strataDefs with overlapping category names)
                    left_join(measurementScale_lookup, by = "stratumMeasurement") %>% 
                    dplyr::select("stratumObservation > plotObservationID" = plotObservationID, 
                                  "stratumObservation > stratumID" = stratumID,
                                  "stratumObservation > stratumMeasurement > value" = stratumMeasurement,
                                  "stratumObservation > stratumMeasurement > attributeID" = attributeID) %>% 
                    distinct() %>% 
                    arrange("stratumObservation > plotObservationID", "stratumObservation > stratumID")
                  
                  # Create nodes
                  stratumObs_nodes = new_vegx_nodes(stratumObs_df, vegx_schema, id_factory)
                  nodes$stratumObservations = append(nodes$stratumObservations, stratumObs_nodes)  
                  
                  # Build lookup table
                  stratumObs_lookup = lapply(nodes$stratumObservations, function(x){
                    data.frame(stratumObservationID = xml2::xml_attr(x, "id"),
                               plotObservationID = xml2::xml_text(xml2::xml_child(x, search = "plotObservationID")),
                               stratumID = xml2::xml_text(xml2::xml_child(x, search = "stratumID")))}) %>% 
                    bind_rows()
                }
                
                if("surfaceCoverObservations" %in% input$observations_input_control){
                  incProgress(progress_increment, "SurfaceCoverObservations")
                  covObs_measurementScale_template = templates() %>% dplyr::filter(template_id == input$covObs_measurementScale)
                  
                  # Check if method exists already 
                  method_name = covObs_measurementScale_template[covObs_measurementScale_template$node_path == "method > name", "node_value"]
                  method_is_quantitative = covObs_measurementScale_template %>% dplyr::filter(main_element == "attributes") %>% pull(node_path) %>% stringr::str_detect("quantitative") %>% all()
                  methods_lookup = data.frame(
                    methodID = sapply(nodes$methods, function(x){xml2::xml_attr(x, "id")}), # The internal id used by vegXshiny
                    methodName = sapply(nodes$methods, function(x){xml2::xml_text(xml2::xml_find_all(x, "..//name"))}) # the mapped unique identifier in the data
                  )
                  
                  if(method_name %in% methods_lookup$methodName){   # Method exists already --> build lookup from nodes
                    method_id = methods_lookup$methodID[which(methods_lookup$methodName == method_name)]
                    attribute_nodes = nodes$attributes[sapply(nodes$attributes, function(x){xml2::xml_attr(x, "id") == method_id})]
                    
                    if(method_is_quantitative){
                      measurementScale_lookup = data.frame(attributeID_measurement = xml2::xml_attr(attribute_nodes[[1]], "id"),
                                                           surfaceCoverMeasurement = data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceCoverMeasurement]) %>% 
                        dplyr::distinct()
                    } else {
                      measurementScale_lookup = lapply(attribute_nodes, function(x){
                        data.frame(attributeID_measurement = xml2::xml_attr(x, "id"),
                                   surfaceCoverMeasurement = xml2::xml_text(xml2::xml_find_first(x, "..//code")))}) %>% 
                        bind_rows()
                    }
                  } else {       # Method does not exist --> build from scratch (if it's not quantitative)
                    if(!method_is_quantitative){
                      node_path = covObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_path) %>% unique()
                      codes_template = covObs_measurementScale_template %>% dplyr::filter(stringr::str_ends(node_path, "code")) %>% pull(node_value)
                      codes_observed = sort(unique(data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceCoverMeasurement]))
                      codes_unmatched = setdiff(codes_observed, codes_template)
                      
                      if(length(codes_unmatched) != 0){  # Check if observations contain undefined measurement categories
                        attributes_addendum = data.frame(template_id = covObs_measurementScale_template$template_id[1], 
                                                         main_element = "attributes", 
                                                         node_path = !!node_path, 
                                                         node_value = codes_unmatched,
                                                         group_value = codes_unmatched) %>% 
                          group_by(group_value) %>% 
                          group_modify(~add_row(.x, template_id = covObs_measurementScale_template$template_id[1], main_element = "attributes", node_path = node_path, node_value = "1")) %>%
                          mutate(node_id = cur_group_id()+max(covObs_measurementScale_template$node_id)) %>% 
                          ungroup() %>% 
                          dplyr::select(template_id, node_id, main_element, node_path, node_value)
                      }
                      covObs_measurementScale_template = bind_rows(covObs_measurementScale_template, attributes_addendum)
                    }
                    
                    # 2. Build Nodes
                    covObs_measurementScale_nodes = templates_to_nodes(covObs_measurementScale_template, vegx_schema, id_factory)
                    nodes$methods = append(nodes$methods, covObs_measurementScale_nodes$methods)
                    nodes$attributes = append(nodes$attributes, covObs_measurementScale_nodes$attributes)
                    
                    # 3. Build lookup
                    if(method_is_quantitative){
                      measurementScale_lookup = data.frame(attributeID_measurement = xml2::xml_attr(covObs_measurementScale_nodes$attributes[[1]], "id"),
                                                           surfaceCoverMeasurement = data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceCoverMeasurement]) %>% 
                        dplyr::distinct()
                    } else {
                      measurementScale_lookup = lapply(covObs_measurementScale_nodes$attributes, function(x){
                        data.frame(attributeID_measurement = xml2::xml_attr(x, "id"),
                                   surfaceCoverMeasurement = xml2::xml_text(xml2::xml_find_first(x, "..//code")))}) %>% 
                        bind_rows()
                    }
                  }
                  
                  # Build surface types
                  surface_types = unique(data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceType])
                  surfaceType_df = data.frame("surfaceType > surfaceName" = surface_types, check.names = F)
                  nodes$surfaceTypes = new_vegx_nodes(surfaceType_df, vegx_schema, id_factory)
                  
                  surfaceTypes_lookup = lapply(nodes$surfaceTypes, function(x){
                    data.frame(attributeID_type = xml2::xml_attr(x, "id"),
                               surfaceType = xml2::xml_text(xml2::xml_find_first(x, "..//surfaceName")))}) %>% 
                    bind_rows()
                  
                  # Build mapping table
                  plotUniqueIdentifier = data_upload[["surfaceCoverObservations"]][,input$covObs_plot_id]
                  if(isTruthy(input$covObs_hasSubplot) && input$covObs_hasSubplot == "yes"){
                    plotUniqueIdentifier = paste(plotUniqueIdentifier, data_upload[["surfaceCoverObservations"]][,input$covObs_subplot_id], sep = "-")
                  }
                  
                  covObs_df = data.frame(plotUniqueIdentifier = plotUniqueIdentifier,
                                         obs_date = lubridate::ymd(data_upload[["surfaceCoverObservations"]][,input$covObs_date]),
                                         surfaceType = data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceType],
                                         surfaceCoverMeasurement = data_upload[["surfaceCoverObservations"]][,input$covObs_surfaceCoverMeasurement]) %>%  
                    left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                    left_join(surfaceTypes_lookup, by = "surfaceType") %>% 
                    left_join(measurementScale_lookup, by = "surfaceCoverMeasurement") %>% 
                    dplyr::select("surfaceCoverObservation > plotObservationID" = plotObservationID, 
                                  "surfaceCoverObservation > surfaceTypeID" = attributeID_type,
                                  "surfaceCoverObservation > surfaceCover > value" = surfaceCoverMeasurement,
                                  "surfaceCoverObservation > surfaceCover > attributeID" = attributeID_measurement) %>% 
                    distinct() %>% 
                    arrange("surfaceCoverObservation > plotObservationID", "surfaceCoverObservation > surfaceTypeID")
                  
                  # Create nodes
                  covObs_nodes = new_vegx_nodes(covObs_df, vegx_schema, id_factory)
                  nodes$surfaceCoverObservations = covObs_nodes
                  
                  # no lookup table needed
                }
                
                if("communityObservations" %in% input$observations_input_control){}
              }
              
              #-------------------------------------------------------------------------#
              # Update app state ####
              # VegX document 
              setProgress(value = 0.8, "Updating VegX document")
              for(element_name in names(nodes)){
                element_nodes = nodes[[element_name]]
                parent_missing = (length(xml_find_all(vegx_doc, paste0("./", element_name))) == 0)
                if(parent_missing){
                  elements_present = xml_root(vegx_doc) %>% xml_children() %>% xml_name()
                  if(length(elements_present) > 0){
                    vegx_main_elements = xml2::xml_attr(xml_children(vegx_schema), "name")
                    elements_ordered = vegx_main_elements[vegx_main_elements %in% c(elements_present, element_name)]
                    insert_position = which(elements_ordered == element_name) - 1
                    xml_add_child(vegx_doc, element_name, .where = insert_position)
                  } else {
                    xml_add_child(vegx_doc, element_name)
                  }
                }
                
                parent = xml_find_all(vegx_doc, paste0("./",  element_name))
                xml_add_child(parent, "placeholder")
                placeholder = xml_child(parent, "placeholder")
                
                for(i in 1:length(element_nodes)){
                  if(!is.null(element_nodes[[i]])){
                    xml_add_sibling(placeholder, element_nodes[[i]], .where = "before", .copy = F)  # This is much faster than xml_add_child()
                  }
                }
                xml_remove(placeholder)  # Remove placeholder
              }
              
              # VegX text 
              vegx_txt(as.character(vegx_doc))
              
              # Action log 
              setProgress(value = 1)
              showNotification("Import finished.")
              new_action_log_record(log_path, "Import info", "Data imported from tables.")
              action_log(read_action_log(log_path))
            })  
        }, error = function(e){
          showNotification("Import failed. Please consult the log for more information.")
          new_action_log_record(log_path, "Import error", e$message)
          action_log(read_action_log(log_path))
        }, finally = {
          removeModal()
        })
      }
    )
  })
}
