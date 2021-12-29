#' fileManagement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import rhandsontable
#' @importFrom shiny NS tagList 
mod_fileManagement_ui <- function(id){
  ns <- NS(id)
  
  # fluidPage(
  #   fixedPanel(width = 2, left = 0, 
  #              fileInput("file", "Upload files", accept = ".csv", width = "100%", multiple = T)
  #              )
  #     
  # )
  sidebarLayout(
    sidebarPanel(width = 2,
                 tagList(
                   fileInput(ns("upload"), "Upload files", width = "100%", multiple = T, accept = ".csv")
                 )
    ),
    mainPanel(width = 10, 
              tagList(
                uiOutput(ns("file_grid")),
                hr(),
                rhandsontable::rHandsontableOutput(ns("hot_table"))
              )
    )
  )
}

#' fileManagement Server Functions
#'
#' @noRd 
mod_fileManagement_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    file_names = reactive({
      req(input$upload)
      input$upload$name
    })
    
    # Render file button grid grid
    output$file_grid = renderUI({
      lapply(file_names(), function(file_name){
        actionButton(ns(paste0("button_", file_name)),
                     width = 200, height = 200, class = "btn-success",
                     label = div(icon("file-csv", "fa-9x black"), tags$br(), file_name))
      })
    }) 
    
    # render uploaded user data as rhandsontable
    user_data = reactiveValues()
    observeEvent(input$upload, {
      lapply(file_names(), function(file_name){
        tbl_path = input$upload$datapath[which(input$upload$name == file_name)]
        tbl = read.csv(tbl_path)
        user_data[[file_name]] = rhandsontable::rhandsontable(tbl, useTypes = F)
      })
    })
    
    # Observe Buttons
    observe(
      lapply(file_names(), function(file_name){
        observeEvent(eventExpr = input[[paste0("button_", file_name)]],
                     handlerExpr = {output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_name]])},         
                     ignoreInit = F)
      })
    )
    
    # Return reactive user data
    return(user_data)
  })
}

## To be copied in the UI
# mod_fileManagement_ui("fileManagement_ui_1")

## To be copied in the server
# mod_fileManagement_server("fileManagement_ui_1")
