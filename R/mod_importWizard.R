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
    navs_pill_list(
      id=ns("sidebar"),
      widths = c(2, 10),
      selected = "Data",
      
      #### Data ####
      nav("Data",
          column(
            width = 10, offset = 1,
            h2("Data"),
            tags$p("Describe your data", class = "text-info"),
            hr(),
            tags$p("This import wizard helps you to convert plant community data from a spreadsheet format into VegX."),
            tags$p("Plant community data are usually organized in a table where rows correspond to species, columns correspond to plots and values reflect the cover
                 of a given species in a given plot. Meta data about species or plots may be appended as additional columns or rows, respectively."),
            div(class = "text-center", style = "max-width: 700px; margin-left: auto; margin-right: auto",
                tags$img(src = "www/images/veg_table.png", contentType = "image/png", alt = "Vegetation table", width = "100%"),
                tags$p("Structure of a typical vegetation dataset.", class = "annotation")
            ),
            tags$p("Please use the File Manager to upload separate header, abudance, and species datasets and assign their roles below."),
            uiOutput(ns("data_ui"))
          )
      ),
      
      #### Project ####
      nav("Project",
          column(
            width = 10, offset = 1,
            h2("Project"),
            tags$p("Describe your project and its contributors", class = "text-info"),
            hr(),
            uiOutput(ns("project_ui"))
          )
      ),
      
      #### Plots ####
      nav("Plots",
          column(
            width = 10, offset = 1,
            h2("Plots"),
            tags$p("Match your plot-related header data to VegX elements", class = "text-info"),
            hr(),
            uiOutput(ns("plots_ui"))
          )
      ),
      
      #### Strata ####
      nav("Strata", 
          column(
            width = 10, offset = 1,
            h2("Strata"),
            tags$p("Match your stratum-related header data to VegX elements", class = "text-info"),
            hr(),
            uiOutput(ns("strata_ui"))
          )
      ),
      
      #### Species ####
      nav("Species",         
          column(
            width = 10, offset = 1,
            h2("Species"),
            tags$p("Match your species data data to VegX elements", class = "text-info"),
            hr(),
            uiOutput(ns("species_ui"))
          )
      ),
      
      #### Observations ####
      nav("Observations",  
          column(
            width = 10, offset = 1,
            h2("Observations"),
            tags$p("Import your observation data", class = "text-info"),
            hr(),
            uiOutput(ns("observations_ui"))
          )
      ),
      
      #### Summary ####
      nav("Summary",  
          column(
            width = 10, offset = 1,
            h2("Summary"),
            tags$p("Review your entries", class = "text-info"),
            hr(),
            uiOutput(ns("summary_ui"))
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
    
    #### Data ####
    output$data_ui = renderUI({
      if(length(names(user_data)) == 0){
        dropdown_empty = c("No files found" = "")
      } else {
        dropdown_empty = c("Choose a file" = "")
      }
      fluidRow(
        column(4, selectInput(ns("header_data"), label = "Header *", choices = c(dropdown_empty, names(user_data)))),
        column(4, selectInput(ns("species_data"), label = "Species *", choices = c(dropdown_empty, names(user_data)))),
        column(4, selectInput(ns("cover_data"), label = "Cover", choices = c(dropdown_empty, names(user_data))))
      )
    })
    
    #### Project ####
    output$project_ui = renderUI({
      tagList(
        textInput(inputId = ns("project_title"), label = "Project title *",  width = "100%"),
        tags$div(
          id = "projectAccordion", class = "panel-group", "role" = "tablist",
          tags$div(
            class = "panel panel-default",
            tags$div(
              class = "panel-heading" , "role" = "tab",
              tags$h4(
                class = "panel-title",
                tags$a("More fields", class ="collapsed", 
                       "role"="button", "data-toggle"="collapse", "data-parent"="#project_accordion", "href"="#projectDetails", 
                )
              )
            ),
            tags$div(
              id="projectDetails", class="panel-collapse collapse", "role"="tabpanel",
              tags$div(
                class = "panel-body",
                textInput(inputId = ns("project_abstract"), label = "Abstract", width = "100%"),
                textInput(inputId = ns("project_citation"), label = "Citation", width = "100%"),
                tags$label("Main Party"),
                fluidRow(
                  column(width = 4, radioButtons(ns("party_type"), label = "Type", choices = list("Individual", "Organization", "Position"), inline = T)),
                  column(width = 4, textInput(ns("party_name"), "Name", width = "100%")),
                  column(width = 4, textInput(ns("party_role"), "Role", width = "100%"))
                )
              )
            )
          )
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
                    column(3, selectInput(ns("plot_location_method"), label = "Measurement method", choices = c("Select a template" = "", c("A", "B"))))  
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
                    column(3, selectInput(ns("plot_elevation_method"), label = "Measurement method", choices = c("Select a template" = "", elev_methods)))
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
                    column(3, selectInput(ns("plot_shape"), label = "Shape", choices =  c("linear", "rectangle", "polygon", "circle"))),
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
          
        )
      }
    })
    
    #### Summary ####
    output$summary_ui = renderUI({
      tagList()
    })
    
    
    #### Observers ####
    observeEvent(input$previous_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) - 1]))
    observeEvent(input$next_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) + 1]))
    
    observeEvent(
      eventExpr = input$submit, 
      handlerExpr = {
        # build project node
        # build citation nodes
        # build party nodes
        # ...
      })
  })
}
