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
                    )
                  )
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
            div(class = "frame",
                tableOutput(ns("summary_project"))
            ),
            
            h3("Plots"),
            fluidRow(
              column(
                6,
                div(class = "frame",
                    h4("Identifier"), 
                    tableOutput(ns("summary_plot_id"))
                ),
                div(class = "frame",
                    h4("Coordinates"), 
                    tableOutput(ns("summary_plot_coordinates"))
                ),
                div(class = "frame",
                    h4("Elevation"), 
                    tableOutput(ns("summary_plot_elevation"))
                ),
              ),
              column(
                6,
                div(class = "frame",
                    h4("Geometry"), 
                    tableOutput(ns("summary_plot_geometry"))
                ),
                div(class = "frame",
                    h4("Topography"), 
                    tableOutput(ns("summary_plot_topography"))
                ),
                div(class = "frame",
                    h4("Parent material"), 
                    tableOutput(ns("summary_plot_parent_material"))
                )
              )
            )
          )
      )
    ),
    
    # Navigation bar ####
    uiOutput(ns("navigation_ui"))
  )
}

#' importWizard Server Functions
#'
#' @noRd 
mod_importWizard_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Server-wide observers & reactives ####
    # Pull method templates
    location_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "location") %>% pull(template_id, name)
    elevation_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "elevation") %>% pull(template_id, name)
    dimension_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "plot dimension") %>% pull(template_id, name)
    area_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "plot area") %>% pull(template_id, name)
    aspect_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "aspect") %>% pull(template_id, name)
    slope_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "slope") %>% pull(template_id, name)
    strata_methods = templates_lookup %>% filter(target_element == "strata", subject == "strata definition") %>% pull(template_id, name)
    cover_methods = templates_lookup %>% filter(target_element == "methods", subject == "plant cover") %>% pull(template_id, name)
    
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
    sidebar_tabs = c("Data", "Project", "Plots", "Observations", "Species", "Summary")
    
    output$navigation_ui = renderUI({
      if(input$sidebar == "Data"){
        buttons = fluidRow(
          column(width = 12, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right")
          )
        )
      } else if(input$sidebar =="Summary") {
        if(TRUE
           # isTruthy(input$plot_data) &
           # isTruthy(input$cover_data) &
           # isTruthy(input$project_title)
        ){
          buttons = fluidRow(
            column(width = 3, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
            column(width = 6, actionButton(ns("submit"), label = "submit", width = "250px", class = "btn-success center-block"))
          )  
        } else {
          buttons = fluidRow(
            column(width = 3, actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back"), width = "100px", class = "pull-left")),
            column(width = 6, actionButton(ns("submit"), label = "submit", width = "250px", class = "btn-success center-block", disabled = "", title = "Please fill all mandatory fields"))
          )
        }
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
          column(width = 4, selectizeInput(ns("party_type"), label = "Type", choices = c("Individual", "Organization", "Position"), width = "100%"))
        )
      )
    })
    
    #-------------------------------------------------------------------------#
    # Plots ####
    observe({  # This observer prevents re-rendering of the entire UI when user_data changes
      if(!is.null(input$plot_data) & length(names(user_data)) != 0){
        file_selected = input$plot_data # save current selection
        updateSelectizeInput(session, inputId = "plot_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data))) 
      }
      
      if(!is.null(input$subplot_data) & length(names(user_data)) != 0){   # TODO this doesnt update properly when subplots are switched off
        file_selected = input$subplot_data # save current selection
        updateSelectizeInput(session, inputId = "subplot_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data))) 
      }
    })
    
    output$plot_ui = renderUI({
      tagList(
        tags$h4("Main plots"),
        tags$p("Assign a dataset", class = "text-info annotation"),
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
                                        choices = append(list("Select a template" = ""), setNames(as.list(location_methods), names(location_methods))))) 
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
                                        choices = append(list("Select a template" = ""), setNames(as.list(elevation_methods), names(elevation_methods))))) 
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
                  column(4, selectInput(ns("plot_shape"), label = "Shape", choices =  list("rectangle", "linear", "polygon", "circle"))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_dimensions_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(area_methods), names(area_methods))))) 
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_area_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(area_methods), names(area_methods))))) 
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
                                        choices = append(list("Select a template" = ""), setNames(as.list(aspect_methods), names(aspect_methods)))))
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("plot_slope"), label = "Slope", choices = c("Select a column" = "", user_data[[input$plot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_slope_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(slope_methods), names(slope_methods))))) 
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
                  column(4, selectInput(ns("subplot_shape"), label = "Shape", choices =  list("rectangle", "linear", "polygon", "circle"))),
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("subplot_width"), label = "Width", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_length"), label = "Length", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_dimensions_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(area_methods), names(area_methods))))) 
                ),
                hr(style = "margin-top:0px; margin-bottom:15px"),
                fluidRow(
                  column(4, selectInput(ns("subplot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$subplot_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("subplot_area_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(area_methods), names(area_methods))))) 
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
      "stratumObservations" = "stratObs",
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
        tags$p("Assign a dataset", class = "text-info annotation"),
        selectizeInput(ns("indOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        uiOutput(ns("indOrgObs_mapping_ui"))
      )
    })
    
    output$indOrgObs_mapping_ui = renderUI({
      req(input$indOrgObs_data)
      tagList(
        fluidRow(
          column(4, selectInput(ns("indOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders))),
          column(4, selectInput(ns("indOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$indOrgObs_data]]$x$rColHeaders)))
        )
      )
    })
    
    ## aggregateOrganismObservations ####
    output$aggregateOrganismObservations_ui = renderUI({
      tagList(
        div(class = "frame", 
            tags$p(icon("info", class = "icon-padded"), "An observation applying to all occurrences of an organism based on an aggregation factor, e.g. a measurement of the 
                      overall cover/biomass/etc. of a specific taxon in a plot.", class = "text-info annotation no-margin"),
        ),
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
        selectizeInput(ns("aggOrgObs_measurementScale"), label = NULL, 
                       choices = append(list("Select a template" = "", "Undefined (ordinal scale)" = "ordinal", "Undefined (continuous scale)" = "continuous"), 
                                        setNames(as.list(cover_methods), names(cover_methods)))),

        hr(),
        tags$label("Observations *"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        selectizeInput(ns("aggOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        uiOutput(ns("aggOrgObs_mapping_ui"))
      )
    })
    
    output$aggOrgObs_strata_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        tagList(
          tags$p("Which definition was used?", class = "text-info annotation"),
          selectizeInput(ns("aggOrgObs_strataDef"), label = NULL, choices = append(list("Select a template" = "", "Undefined" =  "undefined"), setNames(as.list(strata_methods), names(strata_methods))))  
        )
      }
    })
    
    output$aggOrgObs_taxonStratum_mapping_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        column(4, selectInput(ns("aggOrgObs_taxonStratum"), label = "Taxon stratum *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
      }
    })
    
    output$aggOrgObs_subplot_ui = renderUI({
      req(input$subplot_plot_unique_id, input$subplot_id)
      tagList(
        hr(),
        tags$label("Subplots"),
        br(),
        tags$p("Were observations made at the level of subplots?", class = "text-info annotation"),
        radioButtons(ns("aggOrgObs_hasSubplot"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T)
      )
    })
    
    output$aggOrgObs_subplot_mapping_ui = renderUI({
      if(isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot  == "yes"){
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
    ## Project ####
    output$summary_project = renderUI({
      render_summary_table(c("Title *", "Abstract", "Citation", "Party name", "Party role", "Party type"),
                           c(input$project_title, input$project_abstract, input$project_citation, input$party_name, input$party_role, input$party_type))
    })
    
    ## Plots ####
    output$summary_plot_id = renderUI({
      render_summary_table("Unique identifier *", input$plot_unique_id)
    })
    
    output$summary_plot_coordinates = renderUI({
      render_summary_table(c("X-Coordinate", "Y-Coordinate", "Coordinate Reference System (CRS)", "Measurement method"),
                           c(input$plot_coordinates_x, input$plot_coordinates_y, input$plot_crs, templates_lookup$name[as.numeric(input$plot_location_method)]) )
    })
    
    output$summary_plot_elevation = renderUI({
      render_summary_table(c("Plot elevation", "Measurement method"),
                           c(input$plot_elevation, templates_lookup$name[as.numeric(input$plot_elevation_method)]))
    })
    
    output$summary_plot_geometry = renderUI({
      render_summary_table(c("Plot shape", "Plot area", "Measurement method"), 
                           c(input$plot_shape, input$plot_area, templates_lookup$name[as.numeric(input$plot_area_method)]))
    })
    
    output$summary_plot_topography = renderUI({
      render_summary_table(c("Plot aspect", "Aspect measurement method", "Plot slope", "Slope measurement method"), 
                           c(input$plot_aspect, templates_lookup$name[as.numeric(input$plot_aspect_method)], input$plot_slope, templates_lookup$name[as.numeric(input$plot_slope_method)]))
    })
    
    output$summary_plot_parent_material= renderUI({
      render_summary_table("Parent material", input$plot_parent_material)
    })
    
    #-------------------------------------------------------------------------#
    # Build Nodes ####
    observeEvent(
      eventExpr = input$submit, 
      handlerExpr = {
        showModal(
          modalDialog(tags$h3("Import data"),
                      hr(),
                      div(class = "text-center",
                          tags$p("This will add all mappings to your VegX document."),
                      ),
                      size = "l",
                      footer = tagList(
                        tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                  actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check")))
                      )
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
        mappings = list()
        nodes = list()
        #-------------------------------------------------------------------------#
        ## Project ####
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
          nodes$projects = list(new_vegx_node(vegx_schema, colnames(project_df), project_df[1,], id = NULL, log_path))  
        }
        
        if(isTruthy(input$project_citation)){
          mappings$literatureCitations[["literatureCitation > citationString"]]  = list(value = input$project_citation, source = "Text")
          literatureCitations_df = build_node_values_df(mappings$literatureCitations, user_data)
          nodes$literatureCitations = list(new_vegx_node(vegx_schema, colnames(literatureCitations_df), literatureCitations_df[1,], id = NULL, log_path))
          link_vegx_nodes(nodes$projects[[1]]$node, "project > documentCitationID", nodes$literatureCitations[[1]]$node, vegx_schema, log_path)
        }
        
        if(isTruthy(input$party_name)){
          mappings$parties[[paste0("party > choice > ", tolower(input$party_type), "Name")]] = list(value = input$party_name, source = "Text")
          parties_df = build_node_values_df(mappings$parties, user_data)
          nodes$parties = list(new_vegx_node(vegx_schema, colnames(parties_df), parties_df[1,], id = NULL, log_path))
          link_vegx_nodes(nodes$projects[[1]]$node, "project > personnel > partyID", nodes$parties[[1]]$node, vegx_schema, log_path)
        }
        
        #-------------------------------------------------------------------------#
        ## Plots ####
        if(!is.null(input$plot_unique_id)){ # Check if UI has been rendered already
          # Fetch user data assigned plots
          plots_upload = user_data[[input$plot_data]]
          plots_df_upload = jsonlite::fromJSON(plots_upload$x$data)
          colnames(plots_df_upload) = plots_upload$x$rColHeaders
          plots_df_upload = data.frame(plots_df_upload)
          plots_df_upload[plots_df_upload==""] = NA  
          
          # Check identifiers
          plot_ids = plots_df_upload[[input$plot_unique_id]]
          if("" %in% plot_ids){
            warning("Plot IDs may not contain empty values")
          }
          if(length(plot_ids) != length(unique(plot_ids))){
            warning("Plot IDs must be unique")
          }
          
          # Build mappings table
          plots_df = data.frame(
            "plot > plotName" = plot_ids,
            "plot > plotUniqueIdentifier" = plot_ids,
            check.names = F
          )
          
          if("Coordinates" %in% input$plot_input_control){
            if(isTruthy(input$plot_coordinates_x) && isTruthy(input$plot_coordinates_y) && isTruthy(input$plot_location_method) && isTruthy(input$plot_crs)){
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_location_method) %>% templates_to_nodes(vegx_schema, log_path)
              
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
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_elevation_method) %>% templates_to_nodes(vegx_schema, log_path)
              
              plots_df[["plot > location > verticalCoordinates > elevation > value"]] = plots_df_upload[[plot_elevation]]
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
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_dimension_method) %>% templates_to_nodes(vegx_schema, log_path)
              
              plots_df[["plot > geometry > length > value"]] = plots_df_upload[[input$plot_length]]
              plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
              plots_df[["plot > geometry > width > value"]] = plots_df_upload[[input$plot_width]]
              plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
              
              nodes$methods = append(nodes$methods, method_nodes$methods)
              nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
            }
            if(isTruthy(input$plot_area) && isTruthy(input$plot_area_method)){
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_area_method) %>% templates_to_nodes(vegx_schema, log_path)
              
              plots_df[["plot > geometry > area > value"]] = plots_df_upload[[input$plot_area]]
              plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
              
              nodes$methods = append(nodes$methods, method_nodes$methods)
              nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
            }
          }
          
          if("Topography" %in% input$plot_input_control){
            if(isTruthy(input$plot_aspect) && isTruthy(input$plot_aspect_method)){
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_aspect_method) %>% templates_to_nodes(vegx_schema, log_path)
              
              plots_df[["plot > topography > aspect > value"]] = plots_df_upload[[input$plot_aspect]]
              plots_df[["plot > topography > aspect > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
              
              nodes$methods = append(nodes$methods, method_nodes$methods)
              nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
            }
            if(isTruthy(input$plot_slope) && isTruthy(input$plot_slope_method)){
              method_nodes = templates %>% dplyr::filter(template_id ==  input$plot_slope_method) %>% templates_to_nodes(vegx_schema, log_path)
              
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
          plot_nodes = lapply(1:nrow(plots_df), function(i){
            new_vegx_node(vegx_schema, colnames(plots_df), plots_df[i,], id = NULL, log_path)
          })
          plot_nodes = plot_nodes[which(sapply(plot_nodes, function(x) !is.null(x$node)))] # TODO: add error handling here
          nodes$plots = append(nodes$plots, plot_nodes)
          
          plot_lookup = data.frame(
            plotID = sapply(nodes$plots, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
            plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}) # the mapped unique identifier in the data
          )
          
          #------------------------------------#
          ## Subplots ####
          if(!is.null(input$plot_hasSubplot) && isTruthy(input$subplot_data)){
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
            
            if(length(setdiff(plot_ids, plot_lookup$plotUniqueIdentifier)) != 0){   # TODO: Create nodes for those
              warning("Unknown main plots")
            } else if("" %in% subplot_ids){
              warning("Subplot IDs may not contain empty values")
            } else if(length(plotUniqueIdentifiers) != length(unique(plotUniqueIdentifiers))){
              warning("Combination of PlotID and SubPlotID must be unique")
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
            ) %>% filter(complete.cases(.))
            
            if("Geometry" %in% input$subplot_input_control){
              if(isTruthy(input$subplot_shape)){
                subplots_df[["plot > geometry > shape"]] = input$subplot_shape
              }
              if(isTruthy(input$subplot_length) && isTruthy(input$subplot_width) && isTruthy(input$subplot_dimesion_method)){
                method_nodes = templates %>% dplyr::filter(template_id ==  input$subplot_dimension_method) %>% templates_to_nodes(vegx_schema, log_path)
                
                subplots_df[["plot > geometry > length > value"]] = subplots_df_upload[[input$subplot_length]]
                subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                subplots_df[["plot > geometry > width > value"]] = subplots_df_upload[[input$subplot_width]]
                subplots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
                
                nodes$methods = append(nodes$methods, method_nodes$methods)
                nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
              }
              if(isTruthy(input$subplot_area) && isTruthy(input$subplot_area_method)){
                method_template = templates %>% dplyr::filter(template_id ==  input$subplot_area_method)
                
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
              new_vegx_node(vegx_schema, colnames(subplots_df), subplots_df[i,], id = NULL, log_path)
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
            filter(complete.cases(.)) %>% 
            mutate("plotObservation > projectID" = xml2::xml_attr(nodes$projects[[1]]$node, attr = "id"))
          
          # 2. Check if plots have ids already
          plots_unmatched = setdiff(plotObs_df$plotUniqueIdentifier, 
                                    sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}))
          
          if(length(plots_unmatched) > 0){
            plots_df_addendum =  data.frame("plot > plotName" = plots_unmatched, "plot > plotUniqueIdentifier" = plots_unmatched, check.names = F)
            plot_nodes_addendum = lapply(1:nrow(plots_df_addendum), function(i){
              new_vegx_node(vegx_schema, colnames(plots_df_addendum), plots_df_addendum[i,], id = NULL, log_path)
            })
            nodes$plots = append(nodes$plots, plot_nodes_addendum)
            warning(paste0("Added new nodes for the following plot references in observation data", plots_unmatched)) # TODO: incorporate into messaging system
          }
          
          # 3. Replace mapped plot identifiers with internal ids
          plotObs_df = plotObs_df %>% 
            inner_join(plot_lookup, by = "plotUniqueIdentifier") %>% 
            mutate("plotObservation > plotID" = plotID) %>% 
            dplyr::select(-plotUniqueIdentifier, -plotID)
          
          # 4. Create nodes
          plotObs_nodes = lapply(1:nrow(plotObs_df), function(i){
            new_vegx_node(vegx_schema, colnames(plotObs_df), plotObs_df[i,], id = NULL, log_path)
          })
          nodes$plotObservations = append(nodes$plotObservations, plotObs_nodes)  
          
          # 5. Build lookup table
          plotObs_lookup = lapply(plotObs_nodes, function(x){
            data.frame(plotObservationID = xml2::xml_attr(x$node, "id"),
                       plotID = xml2::xml_text(xml2::xml_child(x$node, search = "plotID")),
                       obs_date = xml2::xml_text(xml2::xml_child(x$node, search = "obsStartDate")))}) %>% 
            bind_rows() %>% 
            left_join(plot_lookup, by = "plotID")
          
          #------------------------------------#
          # Build organismNames and OrganismIdentities
          # 1. Fetch data
          orgNames = c() # Organismnames may come from indOrgObs or aggOrgObs
          if("individualOrganismObservations" %in% input$observations_input_control){
            orgNames = c(orgNames, data_upload[["individualOrganismObservations"]][,input$indOrgObs_taxonName])
          }
          if("aggregateOrganismObservations" %in% input$observations_input_control){
            orgNames = c(orgNames, data_upload[["aggregateOrganismObservations"]][, input$aggOrgObs_taxonName])
          }  
          
          # 2. Build Nodes
          orgNames_df = data.frame("organismName" = unique(orgNames), check.names = F)
          orgNames_nodes = lapply(1:nrow(orgNames_df), function(i){
            new_vegx_node(vegx_schema, colnames(orgNames_df), orgNames_df[i,], id = NULL, log_path)
          })
          
          orgIdentities_df = data.frame("organismIdentity > originalOrganismNameID" = sapply(orgNames_nodes, function(x){xml2::xml_attr(x$node, attr = "id")}), check.names = F)
          orgIdentities_nodes = lapply(1:nrow(orgIdentities_df), function(i){
            new_vegx_node(vegx_schema, colnames(orgIdentities_df), orgIdentities_df[i,], id = NULL, log_path)
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
          
          if(input$aggOrgObs_hasStrata == "yes"){
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
              aggOrgObs_strataDef_template = templates %>% dplyr::filter(template_id == input$aggOrgObs_strataDef)
            }
            
            aggOrgObs_strataDef_nodes = templates_to_nodes(aggOrgObs_strataDef_template, vegx_schema = vegx_schema, log_path = log_path)
            nodes$strata = append(nodes$strata, aggOrgObs_strataDef_nodes$strata)
            nodes$methods = append(nodes$methods, aggOrgObs_strataDef_nodes$methods)
            nodes$attributes = append(nodes$attributes, aggOrgObs_strataDef_nodes$attributes)
          }
          
          
          
          #------------------------------------#
          # Build Observations
          ## AggregatOrganismObservations
          if("aggregateOrganismObservations" %in% input$observations_input_control){
            if(input$aggOrgObs_hasStrata == "yes"){
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
                aggOrgObs_strataDef_template = templates %>% dplyr::filter(template_id == input$aggOrgObs_strataDef)
              }
              
              aggOrgObs_strataDef_nodes = templates_to_nodes(aggOrgObs_strataDef_template, vegx_schema = vegx_schema, log_path = log_path)
              nodes$strata = append(nodes$strata, aggOrgObs_strataDef_nodes$strata)
              nodes$methods = append(nodes$methods, aggOrgObs_strataDef_nodes$methods)
              nodes$attributes = append(nodes$attributes, aggOrgObs_strataDef_nodes$attributes)
            }
            
            
            #------------------#
            if(input$aggOrgObs_measurementScale == "ordinal"){
              # 1. Get unique measurements from observation data
              measurement_values = unique(data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement])
              method_df = data.frame(template_id = 1, node_id = 1, main_element = "methods", 
                                     node_path = c("method > subject", "method > name", "method > description"),
                                     node_value = c("aggregate measurement", "Undefined ordinal measurement scale", "An undefined ordinal measurement scale for aggregateOrganismObservations"))
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
            } else if(input$aggOrgObs_measurementScale == "continuous"){
              method_df = data.frame(template_id = 1, node_id = 1, main_element = "methods", 
                                     node_path = c("method > subject", "method > name", "method > description"),
                                     node_value = c("aggregate measurement", "Undefined quantitative measurement scale", "An undefined quantitative measurement scale for aggregateOrganismObservations"))
              attributes_df = data.frame(template_id = 1, node_id = 2, main_element = "attributes",
                                         node_path = c("attribute > choice > quantitative > methodID", "attribute > choice > quantitative > unit"),
                                         node_value = c("1", "undefined"))
              aggOrgObs_measurementScale_template = bind_rows(method_df, attributes_df)
            } else {
              aggOrgObs_measurementScale_template = templates %>% dplyr::filter(template_id == input$aggOrgObs_measurementScale)
            }
            
            # 2. Build Nodes
            aggOrgObs_measurementScale_nodes = templates_to_nodes(aggOrgObs_measurementScale_template, vegx_schema = vegx_schema, log_path = log_path)
            nodes$methods = append(nodes$methods, aggOrgObs_measurementScale_nodes$methods)
            nodes$attributes = append(nodes$attributes, aggOrgObs_measurementScale_nodes$attributes)
            
            # 3. Build lookup (if qualitative scale was used)
            if(xml2::xml_name(xml2::xml_child(aggOrgObs_measurementScale_nodes$attributes[[1]]$node)) != "quantitative"){ # Any better way to decide whether method is quantitative or not?
              measurementScale_lookup = lapply(aggOrgObs_measurementScale_nodes$attributes, function(x){
                data.frame(attributeID = xml2::xml_attr(x$node, "id"),
                           taxon_measurement = xml2::xml_text(xml2::xml_find_first(x$node, "..//code")))}) %>% 
                bind_rows()
            }
            #------------------#
            browser()
            plotUniqueIdentifier = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_plot_id]
            if(isTruthy(input$aggOrgObs_hasSubplot) && input$aggOrgObs_hasSubplot == "yes"){
              plotUniqueIdentifier = paste(plotUniqueIdentifier, data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_subplot_id], sep = "-")
            }
              
            aggOrgObs_mappings = data.frame(
              plotUniqueIdentifier = plotUniqueIdentifier,
              obs_date = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_date],
              organismName = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonName],
              taxon_measurement = data_upload[["aggregateOrganismObservations"]][,input$aggOrgObs_taxonMeasurement]
            )
            
            aggOrgObs_df = aggOrgObs_mappings %>% 
              left_join(plotObs_lookup, by = c("plotUniqueIdentifier", "obs_date")) %>% 
              left_join(organisms_lookup, by = "organismName") %>% 
              left_join(measurementScale_lookup, by = "taxon_measurement") %>% 
              dplyr::select("aggregateOrganismObservation > plotObservationID" = plotObservationID, 
                            "aggregateOrganismObservation > organismIdentityID" = organismIdentityID, 
                            "aggregateOrganismObservation > aggregateOrganismMeasurement > value" = taxon_measurement, 
                            "aggregateOrganismObservation > aggregateOrganismMeasurement > attributeID" = attributeID)
            
            aggOrgObs_nodes = lapply(1:nrow(aggOrgObs_df), function(i){
              new_vegx_node(vegx_schema, colnames(aggOrgObs_df), aggOrgObs_df[i,], id = NULL, log_path)
            })
            nodes$aggregateOrganismObservations = aggOrgObs_nodes  
          }
          
          if("stratumObservations" %in% input$observations_input_control){}
          if("communityObservations" %in% input$observations_input_control){}
          if("surfaceCoverObservations" %in% input$observations_input_control){}
          
        }
        
        #-------------------------------------------------------------------------#
        # Update app state ####
        # Update VegX document 
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
          
          for(node in element_nodes){
            if(!is.null(node$node)){
              xml_add_child(parent, node$node)
            }
          }
        }
        
        # Update VegX text 
        vegx_txt(as.character(vegx_doc))
        
        # Update action log 
        action_log(read_action_log(log_path))
        
        removeModal()
      }
    )
  })
}
