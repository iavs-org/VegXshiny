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
      selected = "Data",
      
      #### Data ####
      nav(title =  "1. Data", value = "Data",
          column(
            width = 10, offset = 1,
            h2("Data"),
            tags$p("Describe your data", class = "text-info annotation no-margin"),
            hr(),
            tags$p("This import wizard helps you to convert plant community data from a spreadsheet format into VegX."),
            tags$p("Plant community data are usually organized in a table where rows correspond to species, columns correspond to plots and values reflect the cover
                 of a given species in a given plot. Meta data about species or plots may be appended as additional columns or rows, respectively."),
            div(class = "text-center", style = "max-width: 700px; margin-left: auto; margin-right: auto",
                tags$img(src = "www/images/veg_table.png", contentType = "image/png", alt = "Vegetation table", width = "100%"),
                tags$p("Structure of a typical vegetation dataset.", class = "annotation")
            ),
            tags$p("Please use the File Manager to upload separate header, cover, and species datasets and assign their roles below."),
            uiOutput(ns("data_ui"))
          )
      ),
      
      #### Project ####
      nav(title = "2. Project", value = "Project",
          column(
            width = 10, offset = 1,
            h2("Project"),
            tags$p("Describe your project and its contributors", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("project_ui"))
          )
      ),
      
      #### Plots ####
      nav(title = "3. Plots", value = "Plots",
          column(
            width = 10, offset = 1,
            h2("Plots"),
            tags$p("Match your plot-related header data to VegX elements", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("plots_ui"))
          )
      ),
      
      #### Strata ####
      nav(title = "4. Strata", value = "Strata",
          column(
            width = 10, offset = 1,
            h2("Strata"),
            tags$p("Match your stratum-related header data to VegX elements", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("strata_ui"))
          )
      ),
      
      #### Species ####
      nav(title = "5. Species", value = "Species",         
          column(
            width = 10, offset = 1,
            h2("Species"),
            tags$p("Match your species data data to VegX elements", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("species_ui"))
          )
      ),
      
      #### Observations ####
      nav(title = "6. Observations", value = "Observations",  
          column(
            width = 10, offset = 1,
            h2("Observations"),
            tags$p("Import your observation data", class = "text-info annotation no-margin"),
            hr(),
            uiOutput(ns("observations_ui"))
          )
      ),
      
      #### Summary ####
      nav(title = "7. Summary", value = "Summary",
          column(
            width = 10, offset = 1,
            h2("Summary"),
            tags$p("Review your entries", class = "text-info annotation no-margin"),
            hr(),
            
            h3("Data", style = "margin-bottom: 6px"),
            div(class = "frame", tableOutput(ns("summary_data"))), 
            
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
            ),
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
    
    #### Navigation ####
    sidebar_tabs = c("Data", "Project", "Plots", "Strata", "Species", "Observations", "Summary")
    
    output$navigation_ui = renderUI({
      if(input$sidebar == "Data"){
        buttons = fluidRow(
          column(width = 12, actionButton(ns("next_tab"), label = div("Next", icon("angle-right")), width = "100px", class = "pull-right")
          )
        )
      } else if(input$sidebar =="Summary") {
        if(isTruthy(input$header_data) &
           isTruthy(input$cover_data) &
           isTruthy(input$project_title)
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
    
    #### Data ####
    output$data_ui = renderUI({
      if(length(names(user_data)) == 0){
        dropdown_empty = c("No files found" = "")
      } else {
        dropdown_empty = c("Choose a file" = "")
      }
      fluidRow(
        column(4, selectInput(ns("header_data"), label = "Header *", choices = c(dropdown_empty, names(user_data)))),
        column(4, selectInput(ns("cover_data"), label = "Cover *", choices = c(dropdown_empty, names(user_data)))),
        column(4, selectInput(ns("species_data"), label = "Species", choices = c(dropdown_empty, names(user_data))))
      )
    })
    
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
          column(width = 4, selectizeInput(ns("party_type"), label = "Type", choices = list("..." = "", "Individual", "Organization", "Position"), width = "100%"))
        )
      )
    })
    
    #### Plots ####
    output$plots_ui = renderUI({
      if(input$header_data == ""){
        tags$label("No header data assigned.")
      } else {
        elev_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "elevation") %>% pull(template_id, name)
        area_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "plot area") %>% pull(template_id, name)
        aspect_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "aspect") %>% pull(template_id, name)
        slope_methods = templates_lookup %>% dplyr::filter(target_element == "methods", subject == "slope") %>% pull(template_id, name)

        tagList(
          selectizeInput(inputId = ns("plot_unique_id"), label = "Unique identifier *", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders), width = "50%"),
          tags$div(
            id = "plotsAccordion", class = "panel-group", "role" = "tablist",
            ##### Coordinates #####
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "plotsCoordinatesHeading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a("Coordinates",
                         "role"="button", "data-toggle"="collapse", "data-parent"="#plotsAccordion", "href"="#plotsCoordinatesBody",
                  )
                )
              ),
              tags$div(
                id="plotsCoordinatesBody", class="panel-collapse collapse in", "role"="tabpanel",
                tags$div(
                  class = "panel-body",
                  fluidRow(
                    column(3, selectInput(ns("plot_coordinates_x"), label = "X-Coordinate", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))), 
                    column(3, selectInput(ns("plot_coordinates_y"), label = "Y-Coordinate", choices =  c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                    column(3, textInput(ns("plot_crs"), label = "Coordinate reference (CRS)")), 
                    column(3, selectInput(ns("plot_location_method"), label = "Measurement method", choices = c("Select a template" = "", c("A", "B"))))  # TODO add template for coordinates
                  )
                )
              )
            ),
            
            #### Elevation ####
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "plotsElevationHeading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a("Elevation",
                         "role"="button", "data-toggle"="collapse", "data-parent"="#plotsAccordion", "href"="#plotsElevationBody",
                  )
                )
              ),
              tags$div(
                id="plotsElevationBody", class="panel-collapse collapse", "role"="tabpanel",
                tags$div(
                  class = "panel-body",
                  fluidRow(
                    column(3, selectInput(ns("plot_elevation"), label = "Elevation", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),  
                    column(3, selectInput(ns("plot_elevation_method"), label = "Measurement method", choices = list("Select a template" = "", names(elev_methods))))
                  )
                )
              )
            ),
            #### Geometry ####
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "plotsGeometryHeading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a("Geometry", class = "collapsed", 
                         "role"="button", "data-toggle"="collapse", "data-parent"="#plotsAccordion", "href"="#plotsGeometryBody", 
                  )
                )
              ),
              tags$div(
                id="plotsGeometryBody", class="panel-collapse collapse", "role"="tabpanel",
                tags$div(
                  class = "panel-body",
                  fluidRow(
                    column(3, selectInput(ns("plot_shape"), label = "Shape", choices =  list("..." = "", "linear", "rectangle", "polygon", "circle"))),
                    column(3, selectInput(ns("plot_area"), label = "Area", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                    column(3, selectInput(ns("plot_area_method"), label = "Area Method", choices = c("Select a column" = "", area_methods)))
                  )
                )
              )
            ),
            #### Topography ####
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "plotsTopographyHeading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a("Topography", class = "collapsed", 
                         "role"="button", "data-toggle"="collapse", "data-parent"="#plotsAccordion", "href"="#plotsTopographyBody",
                  )
                )
              ),
              tags$div(
                id="plotsTopographyBody", class="panel-collapse collapse", "role"="tabpanel",
                tags$div(
                  class = "panel-body",
                  tags$label("Aspect"),
                  tags$div(
                    class="form-inline frame",
                    tags$div(
                      class = "form-group",
                      tags$label("Value", "for" = ns("plot_aspect_value")),
                      selectInput(ns("plot_aspect_value"), label = NULL, choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))
                    ),
                    tags$div(
                      class = "form-group",
                      tags$label("Method", "for" = ns("plot_aspect_method")),
                      selectInput(ns("plot_aspect_method"), label = NULL, choices = c("Select a template" = "", aspect_methods))  
                    )
                  ), 
                  tags$label("Slope"),
                  tags$div(
                    class="form-inline frame",
                    tags$div(
                      class = "form-group",
                      tags$label("Value", "for" = ns("plot_slope_value")),
                      selectInput(ns("plot_slope_value"), label = NULL, choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))
                    ),
                    tags$div(
                      class = "form-group",
                      tags$label("Method", "for" = ns("plot_slope_method")),
                      selectInput(ns("plot_slope_method"), label = NULL, choices = c("Select a template" = "", slope_methods))  
                    )
                  )
                )
              )
            ),
            #### Parent material ####
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "plotsTopographyHeading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a("ParentMaterial", class = "collapsed", "role"="button", "data-toggle"="collapse", "data-parent"="#plotsAccordion", "href"="#plotsParentMaterialBody")
                )
              ),
              tags$div(
                id="plotsParentMaterialBody", class="panel-collapse collapse", "role"="tabpanel",
                tags$div(
                  class = "panel-body",
                  fluidRow(
                    column(3, selectInput(ns("plot_parent_material"), label = "Parent material", choices = c("Select a column" = "", user_data[[input$header_data]]$x$rColHeaders))),
                  ),
                )
              )
            )
          )
        )
      }
    })
    
    #### Strata ####
    output$strata_ui = renderUI({
      if(input$header_data == ""){
        tags$label("No header data assigned.")
      } else {
        tagList()
      }
    })
    
    #### Species ####
    output$species_ui = renderUI({
      if(input$species_data == ""){
        tags$label("No species data assigned.")
      } else {
        tagList()
      }
    })
    
    #### Observations ####
    output$observations_ui = renderUI({
      if(input$cover_data == ""){
        tags$label("No cover data assigned.")
      } else {
        tagList(
          # Plotobservations
          # Individualorganismobservations
          # Aggregateorganismobservations
          # Speciesobservations 
          # Stratumobservations
          # Surfacecoverobservations
        )
      }
    })
    
    #### Summary ####
    ##### Data #####
    output$summary_data = renderUI({
      render_summary_table(c("Header data *", "Cover data *", "Species data"),
                           c(input$header_data, input$cover_data, input$species_data))
    })
    
    ##### Project #####
    output$summary_project = renderUI({
      render_summary_table(c("Title *", "Abstract", "Citation", "Party name", "Party role", "Party type"),
                           c(input$project_title, input$project_abstract, input$project_citation, input$party_name, input$party_role, input$party_type))
    })
    
    ##### Plots #####
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
                          c(input$plot_aspect_value, templates_lookup$name[as.numeric(input$plot_aspect_method)], input$plot_slope_value, templates_lookup$name[as.numeric(input$plot_slope_method)]))
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
                          tags$p("This will create a new VegX document from your import mappings."),
                          tags$br(),
                          tags$b(tags$p("All existing progress will be overwritten!")),   
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
      })
    
    observeEvent(
      eventExpr = input$confirm_import,
      handlerExpr = {
        mappings = list()
        nodes = list()
        
        mappings$project[["project > title"]] = mapping
        
        if(!is.null(input$project_abstract)){
          mappings$project[["project > abstract"]] = list(value = input$project_abstract, source = "text")
        }
        if(!is.null(input$project_citation)){
          mappings$literatureCitation[["literatureCitation > citationString"]] = list(value = input$project_citation, source = "text")
        }
        if(!is.null(input$party_name)){
          mappings$party[[paste0("party > choice > ", input$party_type, "Name")]] = list(value = input$party_name, source = "text")
          mappings$project[["project > personnel > role"]] = list(value = input$party_role, source = "text")
        }
        
        project_df = build_node_values_df(mappings$project, user_data)
        nodes$projects = new_vegx_node(vegx_schema, colnames(project_df), project_df[1,], id = NULL, log_path)
        
        literatureCitation_df = build_node_values_df(mappings$literatureCitation, user_data)
        nodes$literatureCitations = new_vegx_node(vegx_schema, colnames(literatureCitation_df), literatureCitation_df[1,], id = NULL, log_path)
        
        party_df = build_node_values_df(mappings$party, user_data)
        nodes$parties = new_vegx_node(vegx_schema, colnames(party_df), party_df[1,], id = NULL, log_path)
        
        link_vegx_nodes(nodes$projects$node, "project > documentCitationID", nodes$literatureCitations$node, vegx_schema, log_path)
        
        
        for(elem in vegx_main_elements){
          
          if(! elem %in% names(nodes)){next}
          
        }
        
        
        # Update VegX text 
        vegx_txt(as.character(vegx_doc))
        
        # Update action log 
        action_log(read_action_log(log_path))
      }
    )
  })
}
