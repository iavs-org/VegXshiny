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
    shinyjs::useShinyjs(),
    uiOutput(ns("fileBrowser_ui")),
  )
}

#' fileBrowser Server Functions
#'
#' @noRd 
mod_fileBrowser_server <- function(id, user_data, file_focus, action_log, log_path){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(eventExpr = user_data,
                 handlerExpr = print(user_data))
    
    observe({
      file_focus = file_focus()
      if(!is.null(file_focus)){
        btn_id = ns(paste0("select_", file_focus()))
        session$sendCustomMessage("fileFocusChange", list(fileFocus = file_focus, buttonID = btn_id))  
      }
    })
    
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
              id = ns(paste0("container_", file_name)),
              class = "overlay-button-container",
              actionButton(ns(paste0("select_", file_name)),
                           width = "180px", height = "180px", class = "btn-success btn-overlay no-margin",
                           label = div(icon, tags$br(), file_name)),
              actionButton(inputId = ns(paste0("delete_", file_name)),
                           class = "btn-delete btn-xs", label = "",
                           icon = icon("times"))
            )
            
            # Observe click
            observeEvent(
              ignoreInit = F,
              eventExpr = input[[paste0("select_", file_name)]],
              handlerExpr = {
                file_focus(file_name)
              }
            )
            
            # Observe delete
            observeEvent(
              eventExpr = input[[paste0("delete_", file_name)]],
              handlerExpr = {
                # Remove user data
                .subset2(user_data, "impl")$.values$remove(file_name) # hacky way to remove item from reactiveValues
                output$editor = NULL
                
                # Remove Buttons
                btn_id = ns(paste0("container_", file_name))
                session$sendCustomMessage("deleteFileButton", btn_id)
                
                # Set new file focus
                file_focus(NULL)
                
                # Update action log
                shiny::showNotification(paste0(file_name, " deleted"))
                new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted."))
                action_log(read_action_log(log_path))
              }
            )
            
            return(file_button)
          })
        )
      )
      return(ui)
    })
  })
}