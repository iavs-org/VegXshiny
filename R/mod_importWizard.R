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
      
      #### Data ####
      # nav(title =  "1. Data", value = "Data",
      #     column(
      #       width = 10, offset = 1,
      #       h2("Data"),
      #       tags$p("Describe your data", class = "text-info annotation no-margin"),
      #       hr(),
      #       tags$p("This import wizard helps you to convert plant community data from a spreadsheet format into VegX."),
      #       tags$p("Plant community data are usually organized in a table where rows correspond to species, columns correspond to plots and values reflect the cover
      #            of a given species in a given plot. Meta data about species or plots may be appended as additional columns or rows, respectively."),
      #       div(class = "text-center", style = "max-width: 700px; margin-left: auto; margin-right: auto",
      #           tags$img(src = "www/images/veg_table.png", contentType = "image/png", alt = "Vegetation table", width = "100%"),
      #           tags$p("Structure of a typical vegetation dataset.", class = "annotation")
      #       ),
      #       tags$p("Please use the File Manager to upload separate header, cover, and species datasets and assign their roles below.",
      #              tags$b("Note that, contrary to the depiction above, header data should be organized in columns.", class = "text-info"),
      #              "You can use the File Manager to edit your data accordingly."),
      #       uiOutput(ns("data_ui"))
      #     )
      # ),
      # 
      #### Project ####
      nav(title = "1. Project", value = "Project",
          column(
            width = 10, offset = 1,
            h2("Project"),
            tags$p("Describe your project and its contributors", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("project_ui"))
          )
      ),
      
      #### Plots ####
      nav(title = "2. Plots", value = "Plots",
          column(
            width = 10, offset = 1,
            h2("Plots"),
            tags$p("Describe static plot properties", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("plots_ui"))
          )
      ),
      
      
      #### Observations ####
      nav(title = "3. Observations", value = "Observations",  
          column(
            width = 10, offset = 1,
            h2("Observations"),
            tags$p("Import your observation data", class = "text-info annotation no-margin"),
            hr(),
            
            checkboxGroupInput(ns("observations_input_control"), label = "Observation Categories", inline = T, 
                               choiceNames = c("Individual organisms", "Aggregate organisms", "Species", "Stratum", "Community", "Surface cover"),
                               choiceValues = c("individualOrganismObservations", "aggregateOrganismObservations", "speciesObservations", 
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
                    uiOutput(ns("individualOrganismObservation_ui"))
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
                id = ns("speciesObservations"),
                class = "panel panel-default",
                tags$div(
                  id = ns("speciesObservationsHeading"), class = "panel-heading" , "role" = "tab",
                  tags$h4(
                    class = "panel-title",
                    tags$a("SpeciesObservations", class = "collapsed",
                           "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("observationsAccordion")), "href"=paste0("#", ns("speciesObservationsBody"))
                    )
                  )
                ),
                tags$div(
                  id = ns("speciesObservationsBody"), class="panel-collapse collapse", "role"="tabpanel",
                  tags$div(
                    class = "panel-body",
                    uiOutput(ns("speciesObservations_ui"))
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
      
      #### Species ####
      nav(title = "4. Species", value = "Species",         
          column(
            width = 10, offset = 1,
            h2("Species"),
            tags$p("Provide further details on the observed species", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("species_ui"))
          )
      ),
      
      #### Summary ####
      nav(title = "5. Summary", value = "Summary",
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
    
    #### Navigation bar ####
    uiOutput(ns("navigation_ui"))
  )
}

#' importWizard Server Functions
#'
#' @noRd 
mod_importWizard_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dropdown_empty = reactive({
      user_data = isolate(user_data)
      if(length(names(user_data)) == 0){
        c("No files found" = "")
      } else {
        c("Choose a file" = "")
      }
    })
    
    #### Navigation ####
    sidebar_tabs = c("Data", "Project", "Plots", "Observations", "Species", "Summary")
    
    output$navigation_ui = renderUI({
      if(input$sidebar == "Data"){
        buttons = fluidRow(
          column(width = 12, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right")
          )
        )
      } else if(input$sidebar =="Summary") {
        if(TRUE
           # isTruthy(input$header_data) &
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
    #### Project ####
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
    #### Plots ####
    location_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "location") %>% pull(template_id, name)
    elevation_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "elevation") %>% pull(template_id, name)
    area_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "plot area") %>% pull(template_id, name)
    aspect_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "aspect") %>% pull(template_id, name)
    slope_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "slope") %>% pull(template_id, name)
    
    observe({  # This observer prevents re-rendering of the entire UI when user_data changes
      if(!is.null(input$header_data) & length(names(user_data)) != 0){
        file_selected = input$header_data
        updateSelectizeInput(session, inputId = "header_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data))) 
      }
    })
    
    output$plots_ui = renderUI({
      tagList(
        tags$p("Assign a dataset", class = "text-info annotation"),
        selectizeInput(ns("header_data"), label = NULL, choices = c("No files found" = "")),
        uiOutput(ns("plots_mappings_ui"))
      )
    })
    
    output$plots_mappings_ui = renderUI({
      req(input$header_data)
      
      tagList(
        selectizeInput(inputId = ns("plot_unique_id"), label = "Plot ID *", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders)),
        
        checkboxGroupInput(ns("plot_input_control"), label = "Additional plot information", inline = T,
                           choiceNames = c("Coordinates", "Elevation", "Geometry/Area", "Topography", "Parent Material"),
                           choiceValues = c("Coordinates", "Elevation", "Geometry", "Topography", "ParentMaterial")),
        
        tags$div(
          id = ns("plotsAccordion"), class = "panel-group", "role" = "tablist",
          
          ##### Coordinates ####
          tags$div(
            id = ns("plotsCoordinates"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotsCoordinatesHeading") , class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Coordinates",
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotsAccordion")), "href"=paste0("#", ns("plotsCoordinatesBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotsCoordinatesBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectInput(ns("plot_coordinates_x"), label = "X-Coordinate", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))), 
                  column(4, selectInput(ns("plot_coordinates_y"), label = "Y-Coordinate", choices =  c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_location_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(location_methods), names(location_methods))))) 
                ), 
                fluidRow(
                  column(12, textInput(ns("plot_crs"), label = "Coordinate reference string (CRS)", width = "100%"))
                )
              )
            )
          ),
          
          ##### Elevation ####
          tags$div(
            id = ns("plotsElevation"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotsElevationHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Elevation",
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotsAccordion")), "href"=paste0("#", ns("plotsElevationBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotsElevationBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectInput(ns("plot_elevation"), label = "Elevation", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),  
                  column(4, selectInput(ns("plot_elevation_method"), label = "Measurement method",
                                        choices = append(list("Select a template" = ""), setNames(as.list(elevation_methods), names(elevation_methods))))) 
                )
              )
            )
          ),
          
          ##### Geometry ####
          tags$div(
            id = ns("plotsGeometry"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotsGeometryHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Geometry", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotsAccordion")), "href"=paste0("#", ns("plotsGeometryBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotsGeometryBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(4, selectInput(ns("plot_shape"), label = "Shape", choices =  list("rectangle", "linear", "polygon", "circle"))),
                  column(4, selectInput(ns("plot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                  column(4, selectInput(ns("plot_area_method"), label = "Measurement method", 
                                        choices = append(list("Select a template" = ""), setNames(as.list(area_methods), names(area_methods))))) 
                )
              )
            )
          ),
          ##### Topography ####
          tags$div(
            id = ns("plotsTopography"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotsTopographyHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Topography", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotsAccordion")), "href"=paste0("#", ns("plotsTopographyBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotsTopographyBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                tags$label("Aspect"),
                tags$div(
                  class="form-inline frame",
                  tags$div(
                    class = "form-group",
                    tags$label("Value", "for" = ns("plot_aspect")),
                    selectInput(ns("plot_aspect"), label = NULL, choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))
                  ),
                  tags$div(
                    class = "form-group",
                    tags$label("Measurement method", "for" = ns("plot_aspect_method")),
                    selectInput(ns("plot_aspect_method"), label = NULL, 
                                choices = append(list("Select a template" = ""), setNames(as.list(aspect_methods), names(aspect_methods)))) 
                  )
                ), 
                tags$label("Slope"),
                tags$div(
                  class="form-inline frame",
                  tags$div(
                    class = "form-group",
                    tags$label("Value", "for" = ns("plot_slope")),
                    selectInput(ns("plot_slope"), label = NULL, choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))
                  ),
                  tags$div(
                    class = "form-group",
                    tags$label("Measurement method", "for" = ns("plot_slope_method")),
                    selectInput(ns("plot_slope_method"), label = NULL, 
                                choices = append(list("Select a template" = ""), setNames(as.list(slope_methods), names(slope_methods))))
                  )
                )
              )
            )
          ),
          ##### Parent material ####
          tags$div(
            id = ns("plotsParentMaterial"),
            class = "panel panel-default",
            tags$div(
              id = ns("plotsParentMaterialHeading"), class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("Parent Material", class = "collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"=paste0("#", ns("plotsAccordion")), "href"=paste0("#", ns("plotsParentMaterialBody"))
                )
              )
            ),
            tags$div(
              id = ns("plotsParentMaterialBody"), class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                fluidRow(
                  column(3, selectInput(ns("plot_parent_material"), label = "Parent material", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                )
              )
            )
          )
        )
      )
    })
    
    observe({
      panel_names =  c("Coordinates", "Elevation", "Geometry", "Topography", "ParentMaterial")
      for(panel_name in panel_names){
        if(panel_name %in% input$plot_input_control){
          shinyjs::show(paste0("plots", panel_name))
        } else {
          shinyjs::hide(paste0("plots", panel_name))
        }
      }
    })
    
    #-------------------------------------------------------------------------#
    #### Observations ####
    
    observe({
      panel_names =  c("individualOrganismObservations", "aggregateOrganismObservations", "speciesObservations", "stratumObservations", "communityObservations", "surfaceCoverObservations")
      for(panel_name in panel_names){
        if(panel_name %in% input$observations_input_control){
          shinyjs::show(panel_name)
        } else {
          shinyjs::hide(panel_name)
        }
      }
    })
    
    ##### IndividualOrganismObservations ####
    output$individualOrganismObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ##### AggregateOrganismObservations ####
    strata_methods = templates_lookup %>% filter(subject == "strata definition") %>% pull(template_id, name)
    cover_methods = templates_lookup %>% filter(subject == "plant cover") %>% pull(template_id, name)
    
    observe({  # This observer prevents re-rendering of the entire UI when user_data changes
      if(!is.null(input$aggOrgObs_data) & length(names(user_data)) != 0){
        file_selected = input$aggOrgObs_data
        updateSelectizeInput(session, inputId = "aggOrgObs_data", selected = file_selected, choices = c(dropdown_empty(), names(user_data))) 
      }
    })
    
    output$aggregateOrganismObservations_ui = renderUI({
      tagList(
        tags$label("Strata definitions"),
        br(),
        tags$p("Are observations structured into strata?", class = "text-info annotation"),
        radioButtons(ns("aggOrgObs_hasStrata"), label = NULL, choices = c("yes", "no"), selected = "no", inline = T),
        uiOutput(ns("aggOrgObs_strata_ui")),
        
        hr(),
        tags$label("Cover scale"),
        br(),
        tags$p("Which scale was used to measure plant cover?", class = "text-info annotation"),
        selectizeInput(ns("aggOrgObs_coverScale"), label = NULL, choices = append(list("Select a template" = ""), setNames(as.list(cover_methods), names(cover_methods)))),
        
        hr(),
        tags$label("Observations"),
        br(),
        tags$p("Assign a dataset", class = "text-info annotation"),
        selectizeInput(ns("aggOrgObs_data"), label = NULL, choices = c("No files found" = "")),
        uiOutput(ns("aggOrgObs_observations_ui"))
      )
    })
    
    output$aggOrgObs_strata_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        tagList(
          tags$p("Which definition was used?", class = "text-info annotation"),
          selectizeInput(ns("aggOrgObs_strataDef"), label = NULL, choices = append(list("Select a template" = ""), setNames(as.list(strata_methods), names(strata_methods))))  
        )
      }
    })
    
    output$aggOrgObs_observations_ui = renderUI({
      req(input$aggOrgObs_data)
      tagList(
        fluidRow(
          column(4, selectInput(ns("aggOrgObs_plot_id"), label = "Plot unique ID *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          column(4, selectInput(ns("aggOrgObs_date"), label = "Observation date *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
        ),
        fluidRow(
          column(4, selectInput(ns("aggOrgObs_taxonName"), label = "Taxon name *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders))),
          uiOutput(ns("aggOrgObs_taxonStratum_ui")),
          column(4, selectInput(ns("aggOrgObs_taxonCover"), label = "Cover value *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))    
        )
      )
    })
    
    output$aggOrgObs_taxonStratum_ui = renderUI({
      if(input$aggOrgObs_hasStrata == "yes"){
        column(4, selectInput(ns("aggOrgObs_taxonStratum"), label = "Taxon stratum *", choices = c("Select a column" = "", user_data[[input$aggOrgObs_data]]$x$rColHeaders)))
      }
    })
    
    ##### SpeciesObservations ####
    output$speciesObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ##### stratumObservations ####
    output$stratumObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ##### communityObservations ####
    output$communityObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    ##### surfaceCoverObservations ####
    output$surfaceCoverObservations_ui = renderUI({
      tags$p("not implemented")
    })
    
    #-------------------------------------------------------------------------#
    #### Species ####
    output$species_ui = renderUI({
      tagList(
        h3("Species UI")
      )
    })
    
    #-------------------------------------------------------------------------#
    #### Summary ####
    ##### Project ####
    output$summary_project = renderUI({
      render_summary_table(c("Title *", "Abstract", "Citation", "Party name", "Party role", "Party type"),
                           c(input$project_title, input$project_abstract, input$project_citation, input$party_name, input$party_role, input$party_type))
    })
    
    ##### Plots ####
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
    
    #### Build Nodes ####
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
        ##### Project ####
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
        ##### Plots ####
        if(!is.null(input$plot_unique_id)){ # Check if UI has been rendered already
          # TODO error and warning handling
          plots_method_links = c()
          
          # Input mappings
          mappings$plots[["plot > plotName"]] = list(value = paste0(input$header_data, "$",input$plot_unique_id), source = "File")
          mappings$plots[["plot > plotUniqueIdentifier"]] = list(value = paste0(input$header_data, "$", input$plot_unique_id), source = "File")  
          
          
          if("Coordinates" %in% input$plot_input_control){
            if(isTruthy(input$plot_coordinates_x) && isTruthy(input$plot_coordinates_y) && isTruthy(input$plot_location_method) && isTruthy(input$plot_crs)){
              mappings$plots[["plot > location > horizontalCoordinates > coordinates > valueX"]] = list(value = paste0(input$header_data, "$", input$plot_coordinates_x), source = "File")
              mappings$plots[["plot > location > horizontalCoordinates > coordinates > valueY"]] = list(value = paste0(input$header_data, "$", input$plot_coordinates_y), source = "File")
              mappings$plots[["plot > location > horizontalCoordinates > coordinates > spatialReference"]] = list(value = input$plot_crs, source = "Text")
              plots_method_links["plot > location > horizontalCoordinates > coordinates > attributeID"] = input$plot_location_method
            }
          }
          
          if("Elevation" %in% input$plot_input_control){
            if(isTruthy(input$plot_elevation) && isTruthy(input$plot_elevation_method)){
              mappings$plots[["plot > location > verticalCoordinates > elevation > value"]] = list(value = paste0(input$header_data, "$", input$plot_elevation), source = "File")
              plots_method_links["plot > location > verticalCoordinates > elevation > attributeID"] = input$plot_elevation_method}
          }
          
          if("Geometry" %in% input$plot_input_control){
            if(isTruthy(input$plot_shape)){
              mappings$plots[["plot > geometry > shape"]] = list(value = input$plot_shape, source = "Text")}
            if(isTruthy(input$plot_area) && isTruthy(input$plot_area_method)){
              mappings$plots[["plot > geometry > area > value"]] = list(value = paste0(input$header_data, "$", input$plot_area), source = "File")
              plots_method_links["plot > geometry > area > attributeID"] = input$plot_area_method}
          }
          
          if("Topography" %in% input$plot_input_control){
            if(isTruthy(input$plot_aspect) && isTruthy(input$plot_aspect_method)){
              mappings$plots[["plot > topography > aspect > value"]] = list(value = paste0(input$header_data, "$", input$plot_aspect), source = "File")
              plots_method_links["plot > topography > aspect > attributeID"] = input$plot_aspect_method}
            if(isTruthy(input$plot_slope) && isTruthy(input$plot_slope_method)){
              mappings$plots[["plot > topography > slope > value"]] = list(value = paste0(input$header_data, "$", input$plot_slope), source = "File")
              plots_method_links["plot > topography > slope > attributeID"] = input$plot_slope_method}
          }
          
          if("Parent material" %in% input$plot_input_control){
            if(isTruthy(input$plot_parent_material)){
              mappings$plots[["plot > parentMaterial > value"]] = list(value = paste0(input$header_data, "$", input$plot_parent_material), source = "File")}
          }
          
          # Build plot nodes 
          plots_df = build_node_values_df(mappings$plots, user_data) 
          plots_mapping_nodes = lapply(1:nrow(plots_df), function(i){
            new_vegx_node(vegx_schema, colnames(plots_df), plots_df[i,], id = NULL, log_path)
          })
          
          nodes$plots = append(nodes$plots, plots_mapping_nodes)
          
          # Build method/attribute nodes and link to plots
          if(length(plots_method_links) != 0){
            plots_method_links = sort(plots_method_links) # important for correct order of links
            plots_templates = templates %>% dplyr::filter(template_id %in% plots_method_links)
            plots_method_nodes = templates_to_nodes(plots_templates, vegx_schema, log_path)
            
            # Loop over plot nodes
            lapply(plots_mapping_nodes, function(plot_node){
              for(j in 1:length(plots_method_links)){ # Set links
                link_vegx_nodes(plot_node$node, names(plots_method_links)[j], plots_method_nodes$attributes[[j]]$node, vegx_schema, log_path)   
              }
            })
            nodes$methods = append(nodes$methods,  plots_method_nodes$methods)
            nodes$attributes = append(nodes$attributes,  plots_method_nodes$attributes)  
          }
        }
        
        #-------------------------------------------------------------------------#
        ##### Observations #####
        browser()
        if("aggregateOrganismObservations" %in% input$observations_input_control){
          aggOrgObs_mappings = data.frame(
            observation_type = "aggregateOrganismObservation",
            plot_id = input$aggOrgObs_plot_id,
            date = input$aggOrgObs_date,
            taxon_name = input$aggOrgObs_taxonName,
            taxon_cover = input$aggOrgObs_taxonCover
          )
          
          if(input$aggOrgObs_hasStrata == "yes"){
            aggOrgObs_mappings$taxonStratum = input$aggOrgObs_taxonStratum
            aggOrgObs_strataDef_template = templates %>% dplyr::filter(template_id == input$aggOrgObs_strataDef)
            aggOrgObs_strataDef_nodes = templates_to_nodes(aggOrgObs_strataDef_template, vegx_schema = vegx_schema, log_path = log_path)
          }
          
          if(isTruthy(input$aggOrgObs_coverScale)){
            aggOrgObs_coverScale_template = templates %>% dplyr::filter(template_id == input$aggOrgObs_coverScale)
            aggOrgObs_coverScale_nodes = templates_to_nodes(aggOrgObs_coverScale_template, vegx_schema = vegx_schema, log_path = log_path)
          }
        }
        
        if("individualOrganismObservations" %in% input$observations_input_control){}
        if("speciesObservations" %in% input$observations_input_control){}
        if("stratumObservations" %in% input$observations_input_control){}
        if("communityObservations" %in% input$observations_input_control){}
        if("surfaceCoverObservations" %in% input$observations_input_control){}
        
        # Build plotObservations
        # "plotObservation > plotID" = paste0(input$aggOrgObs_data, "$", input$aggOrgObs$), source = "File")
        # "plotObservation > obsStartDate" = paste0(input$aggOrgObs_data, "$",input$), source = "File")
        # "plotObservation > obsEndDate" = paste0(input$aggOrgObs_data, "$",input$), source = "File")
        # "plotObservation > projectID" = xml_attr(nodes$project, "id"), source = "Text")
        
        build_node_values_df()
        
        # Build organismNames
        
        # Build organismIdentities, link to organismNames
        
        # Build Strata, if available
        
        # Build Observations
        ## AggregatOrganismObservations
        
        # <aggregateOrganismObservation id="570">
        #   <plotObservationID>6</plotObservationID>
        #   <organismIdentityID>51</organismIdentityID>
        #   <stratumObservationID>33</stratumObservationID>
        #   <aggregateOrganismMeasurement>
        #     <value>P</value>
        #     <attributeID>6</attributeID>
        #   </aggregateOrganismMeasurement>
        # </aggregateOrganismObservation>
        
        #-------------------------------------------------------------------------#
        ##### Update app state ####
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
