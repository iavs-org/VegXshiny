#' importWizard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import bslib
mod_importWizard_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    navs_pill_list(
      id=ns("sidebar"),
      widths = c(2, 10),
      selected = "Project",
      
      # Project ####
      nav(title = "1. Project", value = "Project",
          column(
            width = 10, offset = 1,
            h2("Project"),
            tags$p("Describe your project and its contributors", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("project_ui"))
          )
      ),
      
      # Plots ####
      nav(title = "2. Plots", value = "Plots",
          column(
            width = 10, offset = 1,
            h2("Plots"),
            tags$p("Describe static plot properties", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("plot_ui"))
          )
      ),
      
      
      # Observations ####
      nav(title = "3. Observations", value = "Observations",  
          column(
            width = 10, offset = 1,
            h2("Observations"),
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
                    tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "An observation applying to all occurrences of an organism based on an aggregation factor, e.g. a measurement of the overall cover/biomass/etc. of a specific taxon in a plot.")
                  ),
                ),
                tags$div(
                  id = ns("aggregateOrganismObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                  tags$div(
                    class = "panel-body",
                    uiOutput(ns("aggregateOrganismObservations_ui"))
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
                    )
                  )
                ),
                tags$div(
                  id = ns("stratumObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                  tags$div(
                    class = "panel-body",
                    uiOutput(ns("stratumObservations_ui"))
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
                    )
                  )
                ),
                tags$div(
                  id = ns("surfaceCoverObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                  tags$div(
                    class = "panel-body",
                    uiOutput(ns("surfaceCoverObservations_ui"))
                  )
                )
              )
            )
          )
      ),
      
      # Summary ####
      nav(title = "4. Summary", value = "Summary",
          column(
            width = 10, offset = 1,
            h2("Summary"),
            tags$p("Review your entries", class = "text-info annotation no-margin"),
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
}

#' importWizard Server Functions
#'
#' @noRd 
mod_importWizard_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, templates, templates_lookup, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Observers & reactives ####
    # Pull method templates
    methods = reactive({
      list(location = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "location") %>% pull(template_id, name),
           elevation = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "elevation") %>% pull(template_id, name),
           plot_dimension = templates_lookup()  %>% dplyr::filter(target_element == "methods", subject == "plot dimension") %>% pull(template_id, name),
           plot_area = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "plot area") %>% pull(template_id, name),
           aspect = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "aspect") %>% pull(template_id, name),
           slope = templates_lookup() %>% dplyr::filter(target_element == "methods", subject == "slope") %>% pull(template_id, name),
           aggOrgObs = templates_lookup() %>% filter(target_element == "methods", subject %in% c("plant cover", "plant count", "plant frequency", "basal area", "user-defined aggregate measurement")) %>% pull(template_id, name),
           strataDef = templates_lookup() %>% filter(target_element == "strata", subject == "strata definition") %>% pull(template_id, name))
    })
    
    # Observe method inputs and trigger mod_newMethodTemplate when custom template option is selected
    observe_method_input = function(input_name, method_subject){
      if(input[[input_name]] == "custom_template"){
        module_id = paste0("new_method-", sample.int(100000, 1))
        mod_newMethodTemplate_server(module_id, method_subject, templates, templates_lookup)
        mod_newMethodTemplate_ui(ns(module_id))
        updateSelectizeInput(session, input_name, selected = "")
      }
    }
    
    observeEvent(input$plot_location_method, handlerExpr = observe_method_input("plot_location_method", "location"))
    observeEvent(input$plot_elevation_method, handlerExpr = observe_method_input("plot_elevation_method", "elevation"))
    observeEvent(input$plot_dimension_method, handlerExpr = observe_method_input("plot_dimension_method", "plot dimension"))
    observeEvent(input$plot_area_method, handlerExpr = observe_method_input("plot_area_method", "plot area"))
    observeEvent(input$plot_aspect_method, handlerExpr = observe_method_input("plot_aspect_method", "aspect"))
    observeEvent(input$plot_slope_method, handlerExpr = observe_method_input("plot_slope_method", "slope"))
    observeEvent(input$subplot_dimension_method, handlerExpr = observe_method_input("subplot_dimension_method", "plot dimension"))
    observeEvent(input$subplot_area_method, handlerExpr = observe_method_input("subplot_area_method", "plot area"))
    observeEvent(input$aggOrgObs_measurementScale, handlerExpr = observe_method_input("aggOrgObs_measurementScale", "user-defined aggregate measurement"))
    
    # Update method inputs to prevent re-rendering of the entire UI when `methods()` changes
    observeEvent(  
      eventExpr = methods(),
      handlerExpr = {
        updateSelectizeInput(session, inputId = "plot_location_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$location), after = 1))
        updateSelectizeInput(session, inputId = "plot_elevation_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$elevation), after = 1))
        updateSelectizeInput(session, inputId = "plot_dimension_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$plot_dimension), after = 1))
        updateSelectizeInput(session, inputId = "plot_area_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$plot_area), after = 1))
        updateSelectizeInput(session, inputId = "plot_aspect_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$aspect), after = 1))
        updateSelectizeInput(session, inputId = "plot_slope_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$slope), after = 1))
        updateSelectizeInput(session, inputId = "subplot_dimension_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$plot_dimension), after = 1))
        updateSelectizeInput(session, inputId = "subplot_area_method", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$plot_area), after = 1))
        updateSelectizeInput(session, inputId = "aggOrgObs_measurementScale", selected = "", choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(methods()$aggOrgObs), after = 1))
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
          column(width = 12, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right")
          )
        )
      } else if(input$sidebar =="Summary") {
        buttons = fluidRow(
          column(width = 3, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
          column(width = 6, actionButton(ns("submit"), label = "submit", width = "250px", class = "btn-success center-block"))
        )  
      } else {
        buttons = fluidRow(
          column(width = 3, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
          column(width = 3, offset = 6, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right"))
        )
      }
      
      fluidRow(
        column(2),  
        column(
          10,
          column(
            width = 10, offset = 1,
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
      if(!is.null(input$plot_data) & length(names(user_data)) != 0){
        file_selected = input$plot_data # save current selection
        updateSelectizeInput(session, inputId = "plot_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data))) 
      }
      
      if(!is.null(input$plot_hasSubplot) && input$plot_hasSubplot == "yes" && length(names(user_data)) != 0){
        file_selected = input$subplot_data # save current selection
        updateSelectizeInput(session, inputId = "subplot_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data)))
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
                  column(4, selectInput(ns("plot_coordinates_x"), label = "X-Coordinate", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))), 
                  column(4, selectInput(ns("plot_coordinates_y"), label = "Y-Coordinate", choices =  c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_location_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$location), after = 1)))
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
                  column(4, selectInput(ns("plot_elevation"), label = "Elevation", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),  
                  column(4, selectInput(ns("plot_elevation_method"), label = "Measurement method",
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$elevation), after = 1)))
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
                  column(4, selectInput(ns("plot_shape"), label = "Shape", choices =  list("", "rectangle", "linear", "polygon", "circle"))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_dimensions_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$plot_dimension), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_area_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$plot_area), after = 1)))
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
                  column(4, selectInput(ns("plot_aspect"), label = "Aspect", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_aspect_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$aspect), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_slope"), label = "Slope", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_slope_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$slope), after = 1)))
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
                  column(3, selectInput(ns("plot_parent_material"), label = "Parent material", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
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
                  column(4, selectInput(ns("subplot_shape"), label = "Shape", choices =  list("", "rectangle", "linear", "polygon", "circle"))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("subplot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_dimensions_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$plot_dimension), after = 1)))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("subplot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_area_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$plot_area), after = 1)))
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
        for(input_name in input_names){
          if(!is.null(input[[input_name]])){
            updateSelectizeInput(session, inputId = input_name, selected = input[[input_name]], choices = c(dropdown_empty(), names(user_data))) 
          }
        }
      }
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
    #       column(4, selectInput(ns("indOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders))),
    #       column(4, selectInput(ns("indOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders)))
    #     )
    #   )
    # })
    
    ## aggregateOrganismObservations ####
    output$aggregateOrganismObservations_ui = renderUI({
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
        selectizeInput(ns("aggOrgObs_measurementScale"), label = NULL, choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$aggOrgObs), after = 1)),
        
        hr(),
        tags$label("Observations *"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        tags$i(class = "glyphicon glyphicon-info-sign", class = "icon-info text-info", title = "A long format table where each aggregate measurement is identified by a unique combination of plot, subplot (optional), date, taxon and stratum (optional)"),
        selectizeInput(ns("aggOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        
        uiOutput(ns("aggOrgObs_mapping_ui"))
      )
    })
    
    output$aggOrgObs_strata_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        tagList(
          tags$p("Which definition was used?", class = "text-info annotation"),
          selectizeInput(ns("aggOrgObs_strataDef"), label = NULL, 
                         choices = append(list("Select a template" = "", "\u2795 define custom method" = "custom_template"), as.list(isolate(methods())$strataDef), after = 1))
        )
      }
    })
    
    output$aggOrgObs_taxonStratum_mapping_ui = renderUI({
      if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
        column(4, selectInput(ns("aggOrgObs_taxonStratum"), label = "Taxon stratum *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
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
        column(4, selectInput(ns("aggOrgObs_subplot_id"), label = "Subplot ID *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
      }
    })
    
    output$aggOrgObs_mapping_ui = renderUI({
      req(input$aggOrgObs_data)
      tagList(
        fluidRow(
          column(4, selectInput(ns("aggOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          uiOutput(ns("aggOrgObs_subplot_mapping_ui")),
          column(4, selectInput(ns("aggOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
        ),
        fluidRow(
          column(4, selectInput(ns("aggOrgObs_taxonName"), label = "Taxon name *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          uiOutput(ns("aggOrgObs_taxonStratum_mapping_ui")),
          column(4, selectInput(ns("aggOrgObs_taxonMeasurement"), label = "Measurement value *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))    
        )
      )
    })
    
    ## stratumObservations ####
    output$stratumObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ## communityObservations ####
    output$communityObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ## surfaceCoverObservations ####
    output$surfaceCoverObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
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
        inputs_complete$plot_coordinates = T  # Set to completeness to TRUE if UI is not rendered
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
        inputs_complete$plot_elevation = T  # Set to completeness to TRUE if UI is not rendered
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
        inputs_complete$plot_geometry = T  # Set to completeness to TRUE if UI is not rendered
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
        inputs_complete$subplot_geometry = T  # Set to completeness to TRUE if UI is not rendered
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
        inputs_complete$plot_topography = T  # Set to completeness to TRUE if UI is not rendered
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
        inputs_complete$parent_material = T  # Set to completeness to TRUE if UI is not rendered
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
        uiOutput(ns("summary_aggOrgObs"))
      }
    })
    
    output$summary_aggOrgObs = renderUI({
      if("aggregateOrganismObservations" %in% input$observations_input_control){
        input_values = list("Plot" = input$aggOrgObs_plot_id, "Subplot" = input$aggOrgObs_subplot_id, "Observation date" = input$aggOrgObs_date, "Taxon name" = input$aggOrgObs_taxonName, 
                            "Stratum definition" = ifelse(isTruthy(input$aggOrgObs_strataDef), templates_lookup()$name[templates_lookup()$template_id == input$aggOrgObs_strataDef], ""), "Stratum" = input$aggOrgObs_taxonStratum, 
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
        
        inputs_complete$aggOrgObs = check_input_completeness(values = values, values_mandatory = 1:length(values))
        render_mapping_summary(header = "AggregateOrganismObservations", labels = labels, values = values, inputs_complete = inputs_complete$aggOrgObs)
      } else {
        inputs_complete$aggOrgObs = T  # Set to completeness to TRUE if UI is not rendered
        return(NULL)
      }
    })
    
    #-------------------------------------------------------------------------#
    # Build Nodes ####
    observeEvent(
      eventExpr = input$submit, 
      handlerExpr = {
        #-------------------------------------------------------------------------#
        ### Check inputs
        # Build UI elements
        if(all(sapply(reactiveValuesToList(inputs_complete), isTRUE))){
          modal_content = div(class = "text-center text-info", icon("check"), tags$p("This will add all mappings to your VegX document."))
          modal_footer = tagList(
            tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                      actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check")))
          )
        } else {
          modal_content = div(class = "text-center text-danger", icon("exclamation"), tags$p("Submission incomplete. Please review your entries."))
          modal_footer = tagList(tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                           shinyjs::disabled(actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check"))))
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
          #-------------------------------------------------------------------------#
          withProgress(
            message = "Importing data",
            expr = {
              # Preparations 
              shinyjs::disable("confirm_import")
              shinyjs::disable("dismiss_modal")
              mappings = list()
              nodes = list()  
              
              ### Project ####
              setProgress(value = 0.05, "Projects")
              if(isTruthy(input$project_title)){
                mappings$project[["project > title"]] = list(value = input$project_title, source = "Text")
              }
              if(isTruthy(input$project_abstract)){
                mappings$project[["project > abstract"]] = list(value = input$project_abstract, source = "Text")
              }
              if(isTruthy(input$party_name) & isTruthy(input$party_role)){
                mappings$project[["project > personnel > role"]] = list(value = input$party_role, source = "Text")
              }
              if(length(mappings$project) != 0){
                project_df = build_node_values_df(mappings$project, user_data) 
                nodes$projects = list(new_vegx_node(colnames(project_df), project_df[1,], id = NULL, log_path, vegx_schema, write_log = F))  
              }
              
              if(isTruthy(input$project_citation)){
                mappings$literatureCitations[["literatureCitation > citationString"]]  = list(value = input$project_citation, source = "Text")
                literatureCitations_df = build_node_values_df(mappings$literatureCitations, user_data)
                nodes$literatureCitations = list(new_vegx_node(colnames(literatureCitations_df), literatureCitations_df[1,], id = NULL, log_path, vegx_schema, write_log = F))
                link_vegx_nodes(nodes$projects[[1]]$node, "project > documentCitationID", nodes$literatureCitations[[1]]$node, log_path, vegx_schema)
              }
              
              if(isTruthy(input$party_name)){
                mappings$parties[[paste0("party > choice > ", tolower(input$party_type), "Name")]] = list(value = input$party_name, source = "Text")
                parties_df = build_node_values_df(mappings$parties, user_data)
                nodes$parties = list(new_vegx_node(colnames(parties_df), parties_df[1,], id = NULL, log_path, vegx_schema, write_log = F))
                link_vegx_nodes(nodes$projects[[1]]$node, "project > personnel > partyID", nodes$parties[[1]]$node, log_path, vegx_schema)
              }
              
              #-------------------------------------------------------------------------#
              ## Plots ####
              if(!is.null(input$plot_unique_id)){ # Check if UI has been rendered already
                # Fetch user data assigned to plots
                setProgress(value = 0.1, "Plots")
                plots_upload = user_data[[input$plot_data]]
                plots_df_upload = jsonlite::fromJSON(plots_upload$x$data)
                colnames(plots_df_upload) = plots_upload$x$rColHeaders
                plots_df_upload = data.frame(plots_df_upload)
                plots_df_upload[plots_df_upload==""] = NA  
                
                # Check identifiers
                plot_ids = plots_df_upload[[input$plot_unique_id]]
                if("" %in% plot_ids){
                  stop("Plot IDs may not contain empty values")
                }
                if(length(plot_ids) != length(unique(plot_ids))){
                  stop("Plot IDs must be unique")
                }
                
                # Build mappings table
                plots_df = data.frame(
                  "plot > plotName" = plot_ids,
                  "plot > plotUniqueIdentifier" = plot_ids,
                  check.names = F
                )
                
                if("Coordinates" %in% input$plot_input_control){
                  if(isTruthy(input$plot_coordinates_x) && isTruthy(input$plot_coordinates_y) && isTruthy(input$plot_location_method) && isTruthy(input$plot_crs)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_location_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > location > horizontalCoordinates > coordinates > valueX"]] = plots_df_upload[[input$plot_coordinates_x]]
                    plots_df[["plot > location > horizontalCoordinates > coordinates > valueY"]] = plots_df_upload[[input$plot_coordinates_y]]
                    plots_df[["plot > location > horizontalCoordinates > coordinates > spatialReference"]] = input$plot_crs
                    plots_df[["plot > location > horizontalCoordinates > coordinates > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Elevation" %in% input$plot_input_control){
                  if(isTruthy(input$plot_elevation) && isTruthy(input$plot_elevation_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_elevation_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > location > verticalCoordinates > elevation > value"]] = plots_df_upload[[input$plot_elevation]]
                    plots_df[["plot > location > verticalCoordinates > elevation > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Geometry" %in% input$plot_input_control){
                  if(isTruthy(input$plot_shape)){
                    plots_df[["plot > geometry > shape"]] = input$plot_shape
                  }
                  if(isTruthy(input$plot_length) && isTruthy(input$plot_width) && isTruthy(input$plot_dimesion_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_dimension_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > geometry > length > value"]] = plots_df_upload[[input$plot_length]]
                    plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    plots_df[["plot > geometry > width > value"]] = plots_df_upload[[input$plot_width]]
                    plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                  if(isTruthy(input$plot_area) && isTruthy(input$plot_area_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_area_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > geometry > area > value"]] = plots_df_upload[[input$plot_area]]
                    plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Topography" %in% input$plot_input_control){
                  if(isTruthy(input$plot_aspect) && isTruthy(input$plot_aspect_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_aspect_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > topography > aspect > value"]] = plots_df_upload[[input$plot_aspect]]
                    plots_df[["plot > topography > aspect > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                  if(isTruthy(input$plot_slope) && isTruthy(input$plot_slope_method)){
                    method_nodes = templates() %>% dplyr::filter(template_id ==  input$plot_slope_method) %>% templates_to_nodes(vegx_schema, log_path)
                    
                    plots_df[["plot > topography > slope > value"]] = plots_df_upload[[input$plot_slope]]
                    plots_df[["plot > topography > slope > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                    
                    nodes$methods = append(nodes$methods, method_nodes$methods)
                    nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                  }
                }
                
                if("Parent material" %in% input$plot_input_control){
                  if(isTruthy(input$plot_parent_material)){
                    plots_df[["plot > parentMaterial > value"]] =plots_df_upload[[input$plot_parent_material]]
                  }
                }
                
                # Build plot nodes 
                vegx_schema_plots = xml_find_all(vegx_schema, "./xsd:element[@name='plots']")
                plot_nodes = lapply(1:nrow(plots_df), function(i){
                  new_vegx_node(colnames(plots_df), plots_df[i,], id = NULL, log_path, vegx_schema_plots, write_log = F)
                })
                plot_nodes = plot_nodes[which(sapply(plot_nodes, function(x) !is.null(x$node)))] # TODO: add error handling here
                nodes$plots = append(nodes$plots, plot_nodes)
                
                plot_lookup = data.frame(
                  plotID = sapply(nodes$plots, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
                  plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}) # the mapped unique identifier in the data
                )
                
                #------------------------------------#
                ## Subplots ####
                if(!is.null(input$plot_hasSubplot) && input$plot_hasSubplot == "yes" && isTruthy(input$subplot_data)){
                  # Fetch user data assigned plots
                  subplots_upload = user_data[[input$subplot_data]]
                  subplots_df_upload = jsonlite::fromJSON(subplots_upload$x$data)
                  colnames(subplots_df_upload) = subplots_upload$x$rColHeaders
                  subplots_df_upload = data.frame(subplots_df_upload)
                  subplots_df_upload[subplots_df_upload==""] = NA  
                  
                  # Build mappings table
                  plot_ids = subplots_df_upload[[input$subplot_plot_unique_id]]
                  subplot_ids = subplots_df_upload[[input$subplot_id]]
                  plotUniqueIdentifiers = paste(subplots_df_upload[[input$subplot_plot_unique_id]], subplots_df_upload[[input$subplot_id]], sep = "-")
                  
                  if(length(setdiff(plot_ids, plot_lookup$plotUniqueIdentifier)) != 0){
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
                      left_join(plot_lookup, by = "plotUniqueIdentifier") %>% 
                      pull(plotID),          
                    "plot > relatedPlot > plotRelationship" = "subplot",
                    check.names = F
                  ) %>% filter(stats::complete.cases(.))
                  
                  if("Geometry" %in% input$subplot_input_control){
                    if(isTruthy(input$subplot_shape)){
                      subplots_df[["plot > geometry > shape"]] = input$subplot_shape
                    }
                    if(isTruthy(input$subplot_length) && isTruthy(input$subplot_width) && isTruthy(input$subplot_dimesion_method)){
                      method_nodes = templates() %>% dplyr::filter(template_id ==  input$subplot_dimension_method) %>% templates_to_nodes(vegx_schema, log_path)
                      
                      subplots_df[["plot > geometry > length > value"]] = subplots_df_upload[[input$subplot_length]]
                      subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                      subplots_df[["plot > geometry > width > value"]] = subplots_df_upload[[input$subplot_width]]
                      subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                      
                      nodes$methods = append(nodes$methods, method_nodes$methods)
                      nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                    }
                    if(isTruthy(input$subplot_area) && isTruthy(input$subplot_area_method)){
                      method_template = templates() %>% dplyr::filter(template_id ==  input$subplot_area_method)
                      
                      # Check if method exists already 
                      method_name = method_template[method_template$node_path == "method > name", "node_value"] 
                      methods_lookup = data.frame(
                        methodID = sapply(nodes$methods, function(x){xml2::xml_attr(x$node, "id")}), # The internal id used by vegXshiny
                        methodName = sapply(nodes$methods, function(x){xml2::xml_text(xml2::xml_find_all(x$node, "..//name"))}) # the mapped unique identifier in the data
                      )
                      
                      if(method_name %in% methods_lookup$methodName){  # If yes, use existing ID
                        attributes_method_links = sapply(nodes$attributes, function(x){xml2::xml_text(xml2::xml_find_all(x$node, "..//methodID"))})
                        attribute_node = nodes$attributes[[which(attributes_method_links == methods_lookup$methodID[methods_lookup$methodName == method_name])]]
                        attribute_id = xml2::xml_attr(attribute_node$node, "id")
                      } else {                                         # If no, create new nodes
                        method_nodes = templates_to_nodes(method_template, vegx_schema, log_path)
                        nodes$methods = append(nodes$methods, method_nodes$methods)
                        nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
                        attribute_id = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                      }
                      
                      subplots_df[["plot > geometry > area > value"]] = subplots_df_upload[[input$subplot_area]]
                      subplots_df[["plot > geometry > area > attributeID"]] = attribute_id
                    }
                  }
                  
                  # Build subplot nodes 
                  subplot_nodes = lapply(1:nrow(subplots_df), function(i){
                    new_vegx_node(colnames(subplots_df), subplots_df[i,], id = NULL, log_path, vegx_schema_plots, write_log = F)
                  })
                  nodes$plots = append(nodes$plots, subplot_nodes)
                  
                  # Update plot lookup
                  plot_lookup = data.frame(
                    plotID = sapply(nodes$plots, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
                    plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}) # the mapped unique identifier in the data
                  )
                }
              }
              
              #-------------------------------------------------------------------------#
              ## Observations ####
              # The central element in VegX is the plotObervation, which is referenced by all other observationTypes. 
              # Additionally, a number of other elements such as methods, organismNames, strata etc. may be shared by different observationTypes.
              # This provides a logical order for building up the VegX document when importing observations: First, build plotObservations from
              # all unique combinations of plot, subplot (if provided) and date across all observations. Second, systematically process potentially shared elements and 
              # create new nodes from the mappings for different observationTypes. Finally, resolve the mapped values to their corresponding 
              # node IDs and create the actual observation elements. 
              #
              # The workflow below follows this rationale.
              if(!is.null(input$observations_input_control) && input$observations_input_control != ""){
                setProgress(value = 0.2, "Plot observations")
                # Fetch user data assigned in observation mappings
                data_upload = sapply(input$observations_input_control, function(obs_category){
                  input_value = input[[paste0(abbreviations[obs_category], "_data")]]
                  file_name = str_split(input_value, "\\$", simplify = T)[1]
                  upload = user_data[[file_name]]
                  df_upload = jsonlite::fromJSON(upload$x$data)
                  colnames(df_upload) = upload$x$rColHeaders
                  return(data.frame(df_upload))
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
                    stop(paste0("Could not parse date field for ", obs_category))
                  })
                  
                  # Get Identifiers
                  plotUniqueIdentifier = df_upload[,input[[paste0(abbreviations[obs_category], "_plot_id")]]]
                  if(isTruthy(input$plot_hasSubplot) && input$plot_hasSubplot == "yes"){
                    plotUniqueIdentifier = paste(plotUniqueIdentifier, df_upload[,input[[paste0(abbreviations[obs_category], "_subplot_id")]]], sep = "-")
                  }
                  date = df_upload[,input[[paste0(abbreviations[obs_category], "_date")]]]
                  
                  return(data.frame("plotUniqueIdentifier" = plotUniqueIdentifier, "plotObservation > obsStartDate" = date, check.names = F))
                }) 
                
                plotObs_df = plotObs_df_list %>% 
                  bind_rows() %>% 
                  distinct() %>% 
                  filter(stats::complete.cases(.)) %>% 
                  mutate("plotObservation > projectID" = xml2::xml_attr(nodes$projects[[1]]$node, attr = "id"))
                
                # 2. Check if plots have ids already
                plots_unmatched = setdiff(plotObs_df$plotUniqueIdentifier, 
                                          sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}))
                
                if(length(plots_unmatched) > 0){
                  plots_df_addendum =  data.frame("plot > plotName" = plots_unmatched, "plot > plotUniqueIdentifier" = plots_unmatched, check.names = F)
                  plot_nodes_addendum = lapply(1:nrow(plots_df_addendum), function(i){
                    new_vegx_node(colnames(plots_df_addendum), plots_df_addendum[i,], id = NULL, log_path, vegx_schema_plots, write_log = F)
                  })
                  nodes$plots = append(nodes$plots, plot_nodes_addendum)
                  warning(paste0("Observation data referenced unknown plots. Added ", length(plots_unmatched)," new plot nodes for the following plots: ", plots_unmatched)) # TODO: incorporate into messaging system
                }
                
                # 3. Replace mapped plot identifiers with internal ids
                plotObs_df = plotObs_df %>% 
                  inner_join(plot_lookup, by = "plotUniqueIdentifier") %>% 
                  mutate("plotObservation > plotID" = plotID) %>% 
                  dplyr::select(-plotUniqueIdentifier, -plotID)
                
                # 4. Create nodes
                vegx_schema_plotObs = xml_find_all(vegx_schema, "./xsd:element[@name='plotObservations']")
                plotObs_nodes = lapply(1:nrow(plotObs_df), function(i){
                  new_vegx_node(colnames(plotObs_df), plotObs_df[i,], id = NULL, log_path, vegx_schema_plotObs, write_log = F)
                })
                nodes$plotObservations = append(nodes$plotObservations, plotObs_nodes)  
                
                # 5. Build lookup table
                plotObs_lookup = lapply(plotObs_nodes, function(x){
                  data.frame(plotObservationID = xml2::xml_attr(x$node, "id"),
                             plotID = xml2::xml_text(xml2::xml_child(x$node, search = "plotID")),
                             obs_date = lubridate::ymd(xml2::xml_text(xml2::xml_child(x$node, search = "obsStartDate"))))}) %>% 
                  bind_rows() %>% 
                  left_join(plot_lookup, by = "plotID")
                
                #------------------------------------#
                # Build organismNames and OrganismIdentities
                # 1. Fetch data
                setProgress(value = 0.3, "Organisms")
                orgNames = c() # Organismnames may come from indOrgObs or aggOrgObs
                if("individualOrganismObservations" %in% input$observations_input_control){
                  orgNames = c(orgNames, data_upload[["individualOrganismObservations"]][,input$indOrgObs_taxonName])
                }
                if("aggregateOrganismObservations" %in% input$observations_input_control){
                  orgNames = c(orgNames, data_upload[["aggregateOrganismObservations"]][, input$aggOrgObs_taxonName])
                }  
                
                # 2. Build Nodes
                vegx_schema_orgNames = xml_find_all(vegx_schema, "./xsd:element[@name='organismNames']")
                orgNames_df = data.frame("organismName" = unique(orgNames), check.names = F)
                orgNames_nodes = lapply(1:nrow(orgNames_df), function(i){
                  new_vegx_node(colnames(orgNames_df), orgNames_df[i,], id = NULL, log_path, vegx_schema_orgNames, write_log = F)
                })
                
                vegx_schema_orgIdentities = xml_find_all(vegx_schema, "./xsd:element[@name='organismIdentities']")
                orgIdentities_df = data.frame("organismIdentity > originalOrganismNameID" = sapply(orgNames_nodes, function(x){xml2::xml_attr(x$node, attr = "id")}), check.names = F)
                orgIdentities_nodes = lapply(1:nrow(orgIdentities_df), function(i){
                  new_vegx_node(colnames(orgIdentities_df), orgIdentities_df[i,], id = NULL, log_path, vegx_schema_orgIdentities, write_log = F)
                })
                
                nodes$organismNames = orgNames_nodes
                nodes$organismIdentities = orgIdentities_nodes
                
                # 3. Build lookup table
                orgIdentities_lookup = lapply(orgIdentities_nodes, function(x){
                  data.frame(organismIdentityID = xml2::xml_attr(x$node, "id"),
                             originalOrganismNameID = xml2::xml_text(xml2::xml_child(x$node, search = "originalOrganismNameID")))}) %>% 
                  bind_rows()
                
                orgNames_lookup = bind_cols(orgNames_df, orgIdentities_df) %>% 
                  setNames(c("organismName", "originalOrganismNameID")) 
                
                organisms_lookup = left_join(orgIdentities_lookup, orgNames_lookup, by = "originalOrganismNameID")
                
                #------------------------------------#
                # Build Strata, if available
                setProgress(value = 0.4, "Strata")
                if(isTruthy(input$aggOrgObs_hasStrata) && input$aggOrgObs_hasStrata == "yes"){
                  if(input$aggOrgObs_strataDef == "undefined"){
                    stratum_values = unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonStratum])
                    method_df = data.frame(template_id = 1, node_id = 1, main_element = "methods", 
                                           node_path = c("method > subject", "method > name", "method > description"),
                                           node_value = c("stratum definition", "stratum definition/undefined", "An undefined stratum definition"))
                    strata_df = data.frame(template_id = 1, main_element = "strata", 
                                           node_path = "stratum > stratumName", 
                                           node_value = stratum_values,
                                           group_value = stratum_values) %>% 
                      group_by(group_value) %>% 
                      group_modify(~add_row(.x, template_id = 1, main_element = "strata", node_path = "stratum > methodID", node_value = "1")) %>%
                      mutate(node_id = cur_group_id()+1) %>% 
                      ungroup() %>% 
                      dplyr::select(template_id, node_id, main_element, node_path, node_value)
                    
                    aggOrgObs_strataDef_template = bind_rows(method_df, strata_df)
                  } else {
                    aggOrgObs_strataDef_template = templates() %>% dplyr::filter(template_id == input$aggOrgObs_strataDef)
                    
                    # Check if observations contain undefined strata
                    strata_template = aggOrgObs_strataDef_template %>% 
                      dplyr::filter(node_path == "stratum > stratumName") %>% 
                      pull(node_value)
                    strata_observations = unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonStratum])
                    strata_unmatched = setdiff(strata_observations, strata_template)
                    
                    if(length(strata_unmatched) != 0){
                      
                      strata_df = data.frame(template_id = aggOrgObs_strataDef_template$template_id[1], 
                                             main_element = "strata", 
                                             node_path = "stratum > stratumName", 
                                             node_value = strata_unmatched,
                                             group_value = strata_unmatched) %>% 
                        group_by(group_value) %>% 
                        group_modify(~add_row(.x, template_id = aggOrgObs_strataDef_template$template_id[1], main_element = "strata", node_path = "stratum > methodID", node_value = "1")) %>%
                        mutate(node_id = cur_group_id()+max(aggOrgObs_strataDef_template$node_id)) %>% 
                        ungroup() %>% 
                        dplyr::select(template_id, node_id, main_element, node_path, node_value)
                      
                      aggOrgObs_strataDef_template = bind_rows(aggOrgObs_strataDef_template, strata_df)
                    }
                  }
                  
                  aggOrgObs_strataDef_nodes = templates_to_nodes(aggOrgObs_strataDef_template, vegx_schema = vegx_schema, log_path = log_path)
                  nodes$strata = append(nodes$strata, aggOrgObs_strataDef_nodes$strata)
                  nodes$methods = append(nodes$methods, aggOrgObs_strataDef_nodes$methods)
                  nodes$attributes = append(nodes$attributes, aggOrgObs_strataDef_nodes$attributes)
                  
                  strata_lookup = lapply(nodes$strata, function(x){
                    data.frame(stratumID = xml2::xml_attr(x$node, "id"),
                               stratumName = xml2::xml_text(xml2::xml_child(x$node, search = "stratumName")))}) %>% 
                    bind_rows()
                }
                
                #------------------------------------#
                # Build Observations
                progress_increment = 0.4 / (length(input$observations_input_control) + 1)
                ## AggregatOrganismObservations
                incProgress(progress_increment, "Aggregate Organism Observations")
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
                      stratumName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonStratum]
                    ) %>%  
                      left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                      left_join(strata_lookup, by = "stratumName") %>% 
                      dplyr::select("stratumObservation > plotObservationID" = plotObservationID, 
                                    "stratumObservation > stratumID" = stratumID) %>% 
                      distinct() %>% 
                      arrange("stratumObservation > plotObservationID", "stratumObservation > stratumID")
                    
                    # Create nodes
                    vegx_schema_stratumObs = xml_find_all(vegx_schema, "./xsd:element[@name='stratumObservations']")
                    aggOrgObs_stratumObs_nodes = lapply(1:nrow(aggOrgObs_stratumObs_df), function(i){
                      new_vegx_node(colnames(aggOrgObs_stratumObs_df), aggOrgObs_stratumObs_df[i,], id = NULL, log_path, vegx_schema_stratumObs, write_log = F)
                    })
                    nodes$stratumObservations = append(nodes$stratumObservations, aggOrgObs_stratumObs_nodes)  
                    
                    # Build lookup table
                    stratumObs_lookup = lapply(nodes$stratumObservations, function(x){
                      data.frame(stratumObservationID = xml2::xml_attr(x$node, "id"),
                                 plotObservationID = xml2::xml_text(xml2::xml_child(x$node, search = "plotObservationID")),
                                 stratumID = xml2::xml_text(xml2::xml_child(x$node, search = "stratumID")))}) %>% 
                      bind_rows()
                  }
                  
                  #------------------#
                  if(input$aggOrgObs_measurementScale == "quantitative"){
                    method_is_quantitative = T
                    method_df = data.frame(template_id = 1, node_id = 1, main_element = "methods", 
                                           node_path = c("method > subject", "method > name", "method > description"),
                                           node_value = c("aggregate measurement", "Undefined quantitative measurement scale", "An undefined quantitative measurement scale for aggregateOrganismObservations"))
                    attributes_df = data.frame(template_id = 1, node_id = 2, main_element = "attributes",
                                               node_path = c("attribute > choice > quantitative > methodID", "attribute > choice > quantitative > unit"),
                                               node_value = c("1", "undefined"))
                    aggOrgObs_measurementScale_template = bind_rows(method_df, attributes_df)
                  } else if(input$aggOrgObs_measurementScale == "ordinal"){
                    method_is_quantitative = F
                    measurement_values = unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement])  # 1. Get unique measurements from observation data
                    method_df = data.frame(template_id = 1, node_id = 1, main_element = "methods", 
                                           node_path = c("method > subject", "method > name", "method > description"),
                                           node_value = c("aggregate measurement", "aggregate measurement/undefined", "An undefined ordinal measurement scale for aggregateOrganismObservations"))
                    attributes_df = data.frame(template_id = 1, main_element = "attributes", 
                                               node_path = "attribute > choice > ordinal > code", 
                                               node_value = measurement_values,
                                               group_value = measurement_values) %>% 
                      group_by(group_value) %>% 
                      group_modify(~add_row(.x, template_id = 1, main_element = "attributes", node_path = "attribute > choice > ordinal > methodID", node_value = "1")) %>%
                      mutate(node_id = cur_group_id()+1) %>% 
                      ungroup() %>% 
                      select(template_id, node_id, main_element, node_path, node_value)
                    
                    aggOrgObs_measurementScale_template = bind_rows(method_df, attributes_df)
                  } else  {
                    aggOrgObs_measurementScale_template = templates() %>% dplyr::filter(template_id == input$aggOrgObs_measurementScale)
                    method_is_quantitative = aggOrgObs_measurementScale_template %>% dplyr::filter(main_element == "attributes") %>% pull(node_path) %>% stringr::str_detect("quantitative") %>% all()
                    
                    # Check if observations contain undefined measurement categories
                    if(!method_is_quantitative){
                      codes_template = aggOrgObs_measurementScale_template %>% 
                        dplyr::filter(node_path == "attribute > choice > ordinal > code") %>% 
                        pull(node_value)
                      codes_observations = unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement])
                      codes_unmatched = setdiff(codes_observations, codes_template)
                      
                      if(length(codes_unmatched) != 0){
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
                  }
                  
                  # 2. Build Nodes
                  aggOrgObs_measurementScale_nodes = templates_to_nodes(aggOrgObs_measurementScale_template, vegx_schema = vegx_schema, log_path = log_path)
                  nodes$methods = append(nodes$methods, aggOrgObs_measurementScale_nodes$methods)
                  nodes$attributes = append(nodes$attributes, aggOrgObs_measurementScale_nodes$attributes)
                  
                  # 3. Build lookup (if qualitative scale was used)
                  if(method_is_quantitative){
                    measurementScale_lookup = data.frame(attributeID = xml2::xml_attr(aggOrgObs_measurementScale_nodes$attributes[[1]]$node, "id"),
                                                         taxon_measurement = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement])
                  } else {
                    measurementScale_lookup = lapply(aggOrgObs_measurementScale_nodes$attributes, function(x){
                      data.frame(attributeID = xml2::xml_attr(x$node, "id"),
                                 taxon_measurement = xml2::xml_text(xml2::xml_find_first(x$node, "..//code")))}) %>% 
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
                    aggOrgObs_mappings$stratumName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonStratum]
                    
                    aggOrgObs_df = aggOrgObs_mappings %>% 
                      left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
                      left_join(organisms_lookup, by = "organismName") %>% 
                      left_join(measurementScale_lookup, by = "taxon_measurement") %>%
                      left_join(strata_lookup, by = "stratumName") %>% 
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
                  
                  vegx_schema_aggOrgObs = xml_find_all(vegx_schema, "./xsd:element[@name='aggregateOrganismObservations']")
                  aggOrgObs_nodes = lapply(1:nrow(aggOrgObs_df), function(i){
                    new_vegx_node(colnames(aggOrgObs_df), aggOrgObs_df[i,], id = NULL, log_path, vegx_schema_aggOrgObs, write_log = F)
                  })
                  nodes$aggregateOrganismObservations = aggOrgObs_nodes  
                }
                
                if("stratumObservations" %in% input$observations_input_control){}
                if("communityObservations" %in% input$observations_input_control){}
                if("surfaceCoverObservations" %in% input$observations_input_control){}
              }
              
              #-------------------------------------------------------------------------#
              # Update app state ####
              setProgress(value = 0.8, "Updating VegX document")
              # VegX document 
              for(element_name in names(nodes)){
                element_nodes = nodes[[element_name]]
                parent_missing = (length(xml_find_all(vegx_doc, paste0("./", element_name))) == 0)
                if(parent_missing){
                  elements_present = xml_root(vegx_doc) %>% xml_children() %>% xml_name()
                  if(length(elements_present) > 0){
                    elements_ordered = vegx_main_elements[vegx_main_elements %in% c(elements_present, element_name)]
                    insert_position = which(elements_ordered == element_name) - 1
                    xml_add_child(vegx_doc, element_name, .where = insert_position)
                  } else {
                    xml_add_child(vegx_doc, element_name)
                  }
                }
                parent = xml_find_all(vegx_doc, paste0("./",  element_name))
                xml_add_child(parent, element_nodes[[1]]$node)
                
                if(length(element_nodes) > 1){
                  target = xml_child(parent, 1)
                  for(i in 2:length(element_nodes)){
                    if(!is.null(element_nodes[[i]]$node)){
                      xml_add_sibling(target, element_nodes[[i]]$node)  # This is much faster than xml_add_child(), especially as the document grows
                      target = xml_child(parent, i)
                    }
                  }
                }
              }
              
              # VegX text 
              vegx_txt(as.character(vegx_doc))
              
              # Action log 
              setProgress(value = 1)
              showNotification("Import finished")
              new_action_log_record(log_path, "Import info", "Data imported")
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
