#' fileBrowser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fileBrowser_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("fileBrowser_ui")),
  )
}

#' fileBrowser Server Functions
#'
#' @noRd 
mod_fileBrowser_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    browser()
    output$fileBrowser_ui = renderUI({
      req(user_data)
      ui = fluidRow(
        class = "file-grid",
        column(
          12,
          lapply(names(user_data), function(file_name){           # Loop over file names in user_data()
            if(is.null(user_data[[file_name]])){
              return()
            }
            file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
            icon = switch(file_ext,                                       # Assign appropriate file icon
                          "csv" = icon("file-csv", "fa-9x black"),
                          "txt" = icon("file", "fa-9x black"),
                          "xls" = icon("file-excel", "fa-9x black"),
                          "xlsx" = icon("file-excel", "fa-9x black"),
                          "xml" = icon("file-code", "fa-9x black"),
                          icon("file", "fa-9x black"))
            
            # Create Button and Event listener
            file_button = div(
              class = "overlay-button-container",
              actionButton(ns(paste0("select_", file_name)),
                           width = "180px", height = "180px", class = "btn-success btn-overlay no-margin",
                           label = div(icon, tags$br(), file_name)),
              actionButton(inputId = ns(paste0("delete_", file_name)),
                           class = "btn-delete btn-xs", label = "",
                           icon = icon("times"))
            )
            
            return(file_button)
          })
        )
      )
      return(ui)
    })
  })
}



#   observeEvent(
#     ignoreInit = F,
#     eventExpr = input[[paste0("select_", file_name)]],
#     handlerExpr = {
#       shinyjs::removeClass(id = paste0("select_", file_focus()), class = "btn-focus")
#       file_focus(file_name)
#       shinyjs::addClass(id = paste0("select_", file_focus()), class = "btn-focus")
#     }         
#   )
#   
#   observeEvent(
#     eventExpr = input[[paste0("delete_", file_focus())]],
#     handlerExpr = {
#       # Remove user data
#       file_name = isolate(file_focus())
#       .subset2(user_data, "impl")$.values$remove(file_name) # hacky way to remove item from reactiveValues
#       output$editor = NULL
#       
#       # Set new file focus
#       file_focus(NULL)
#       
#       # Redraw file browser
#       
#       # Update action log
#       shiny::showNotification(paste0(file_name, " deleted"))
#       new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted."))
#       action_log(read_action_log(log_path))
#     }
#   )
# return(file_button)


## To be copied in the UI
# mod_fileBrowser_ui("fileBrowser_ui_1")

## To be copied in the server
# mod_fileBrowser_server("fileBrowser_ui_1")
