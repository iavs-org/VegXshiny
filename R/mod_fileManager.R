#' fileManager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom rhandsontable rhandsontable renderRHandsontable
mod_fileManager_ui <- function(id){
  ns <- NS(id)
  
  sidebarLayout(
    tags$div(class = "col-sm-2 well", role = "complementary", 
             fileInput(ns("upload"), "Upload files", width = "100%", multiple = T, accept = c(".csv", ".tv2", ".tv3", ".txt", ".tsv", ".tab", ".rds", ".RData"))
    ),
    
    # TODO Add edit button, save edits (hot_to_r(input$xxx))
    
    mainPanel(
      width = 10, 
      tagList(
        tags$label("File browser"),
        uiOutput(ns("file_browser")),
        tags$hr(),
        tags$label("File Editor"),
        uiOutput(ns("file_editor"))
      )
    )
  )
}

#' fileManager Server Functions
#'
#' @noRd 
mod_fileManager_server <- function(id, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    user_data = reactiveValues()
    file_focus = reactiveVal()
    
    # Render file browser
    output$file_browser = renderUI({
      req(user_data)
      ui = tagList(
        fluidRow(
          class = "file-grid",
          column(
            12,
            lapply(names(user_data), function(file_name){
              file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
              icon = switch(file_ext,
                            "csv" = icon("file-csv", "fa-9x black"),
                            "xls" = icon("file-excel", "fa-9x black"),
                            "xlsx" = icon("file-excel", "fa-9x black"),
                            icon("file-spreadsheet", "fa-9x black"))
              
              file_button = actionButton(ns(paste0("button_", file_name)),
                                         width = "200px", height = "200px", class = "btn-success",
                                         label = div(icon, tags$br(), file_name))
              
              observeEvent(eventExpr = input[[paste0("button_", file_name)]],
                           handlerExpr = {file_focus(file_name)},         
                           ignoreInit = F)
              
              return(file_button)
            })
          )
        )
      )
      return(ui)
    }) 
    
    output$file_editor = renderUI({
      
      req(file_focus())
      output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
      ui = tagList(
        fluidRow(
          column(12,
                 actionButton(ns("edit"), label = "Edit"),
                
                 actionButton(ns("delete"), label = "Delete")
          ),
        ),
        rhandsontable::rHandsontableOutput(ns("hot_table"))
      )
    })
    
    # Render uploaded user data as rhandsontable
    observeEvent(input$upload, {
      lapply(input$upload$name, function(file_name){
        tryCatch(
          expr = {
            file_info = input$upload[which(input$upload$name == file_name),]
            file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
            if(file_ext == "csv"){
              tbl = read.csv(file_info$datapath)
            } else if(file_ext %in% c("tab", "tsv", "txt")){
              tbl = read.delim(file_info$datapath)
            } else if(file_ext %in% c("xls", "xlsx")){
              tbl = readxl::read_excel(file_info$datapath)
            } else (
              stop("Unsupported file format.")
            )
            user_data[[file_name]] = rhandsontable::rhandsontable(tbl, useTypes = F, readOnly = T)
            new_action_log_record(log_path, "Upload info", paste0("File '", file_name,"' uploaded"))
          }, error = function(e){
            shiny::showNotification("Upload failed. Please consult the log for more information.", type = "error")
            new_action_log_record(log_path, "Upload error", paste0("Upload of file '", file_name,"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
          }
        )
      })
      
      # Update log
      action_log(read_action_log(log_path))
    })
    
    # TODO Import TV2/TV3
    
    # Return reactive user data
    return(user_data)
  })
}
