#' fileManagement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom rhandsontable rhandsontable renderRHandsontable
mod_fileManagement_ui <- function(id){
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 tagList(
                   fileInput(ns("upload"), "Upload files", width = "100%", multiple = T, accept = ".csv")
                 )
    ),
    # TODO Add edit button, save edits (hot_to_r(input$xxx))
    
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
mod_fileManagement_server <- function(id, action_log){
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
    
    # Render uploaded user data as rhandsontable
    user_data = reactiveValues()
    observeEvent(input$upload, {
      lapply(file_names(), function(file_name){
        # Create table
        tbl_path = input$upload$datapath[which(input$upload$name == file_name)]
        tbl = read.csv(tbl_path)
        user_data[[file_name]] = rhandsontable::rhandsontable(tbl, useTypes = F)
        
        # Write log entry
        new_action_log_record("Upload info", paste0("File '", file_name,"' uploaded"), session$token)
      })
      # Update log
      log = read_action_log(session$token)
      action_log(log)
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