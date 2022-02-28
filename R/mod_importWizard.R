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
      
      #
      nav("Data",
          h2("Describe your data"),
          div(class = "annotation text-info",
              tags$p("This import wizard helps you to convert plant community data from a spreadsheet format into VegX."),
              tags$p("Plant community data are usually organized in a table where rows correspond to species, columns correspond to plots and values reflect the abundance
                     of a given species in a given plot. Meta data about species or plots may be appended as additional columns or rows, respectively."),
              div(class = "text-center", style = "max-width: 700px; margin-left: auto; margin-right: auto",
                  tags$img(src = "www/images/veg_table.png", contentType = "image/png", alt = "Vegetation table", width = "100%")
              ),
              tags$p("Structure and format of a typical vegetation dataset.", class = "text-center"),
              tags$p("Please use the File Manager to upload separate plot, abudance and species datasets. With the selection boxes below, you can assign files to their corresponding role."),
              
              uiOutput(ns("data_input"))
          ),
      ),
      
      nav("Project",
          h2("Describe the project and involved organisations and individuals."),
          hr(),
          textInput(inputId = ns("project_title"), label = "Project title *",  width = "100%"),
          tags$div(
            id = "accordion", class = "panel-group", "role" = "tablist", "aria-multiselectable" = "true",
            tags$div(
              class = "panel panel-default",
              tags$div(
                id = "Heading", class = "panel-heading" , "role" = "tab",
                tags$h4(
                  class = "panel-title",
                  tags$a(class ="collapsed", "Optional fields",
                         "role"="button", "data-toggle"="collapse", "data-parent"="#accordion", "href"="#collapseProject", "aria-expanded"="false", "aria-controls"="collapseOne",
                  )
                )
              ),
              tags$div(
                id="collapseProject", class="panel-collapse collapse", "role"="tabpanel", "aria-labelledby"="headingOne",
                tags$div(
                  class = "panel-body",
                  textInput(inputId = ns("project_abstract"), label = "Abstract", width = "100%"),
                  textInput(inputId = ns("project_citation"), label = "Citation", width = "100%"),
                  tags$label("Main Party"),
                  fluidRow(
                    column(width = 4, radioButtons(ns("party_type"), label = "Type", choices = list("Individual", "Organization", "Position"), inline = T)),
                    column(width = 4, textInput(ns("party_name"), "Name", width = "100%")),
                    column(width = 4, textInput(ns("party_role"), "Role", width = "100%"))
                  ),
                )
              )
            )
          )
      ),
      
      nav("PlotObservations",
          h2("Associate plot-related header data to VegX elements")
      ),
      
      nav("Strata", 
          h2("Associate your header data to VegX files"),
      ),
      
      nav("Species",  
          
      ),
      
      nav("Individuals",  
          
      ),
      
      nav("Summary",  
          
      )
    ),
    fluidRow(
      column(2),
      column(10,
             hr(),
             tags$span(actionButton(ns("previous_tab"), label = div(icon("angle-left"), "Back")),
                       actionButton(ns("next_tab"), label = div("Next", icon("angle-right")))
             )
      )  
    )
  )
}

#' importWizard Server Functions
#'
#' @noRd 
mod_importWizard_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #### Module-wide variables and observers ####
    sidebar_tabs = c("Data", "Project", "PlotObservations", "Strata", "Species", "Individuals")
    
    observeEvent(input$previous_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) - 1]))
    observeEvent(input$next_tab, nav_select("sidebar", selected = sidebar_tabs[which(sidebar_tabs == input$sidebar) + 1]))
    
    #### Tab1: Data Input ####
    output$data_input = renderUI({
      fluidRow(
        column(4, selectizeInput(ns("header_data"), label = "Header", choices = names(user_data))),
        column(4, selectizeInput(ns("abundances_data"), label = "Abundances", choices = names(user_data))),
        column(4, selectizeInput(ns("species_data"), label = "Species", choices = names(user_data)))
      )
    })

  })
}
