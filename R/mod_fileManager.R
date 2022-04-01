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
    
    #### Upload ####
    # Process and render uploaded files
    observeEvent(input$upload, {
      lapply(input$upload$name, function(file_name){      # TODO Import TV2/TV3
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
            
            user_data[[file_name]] = rhandsontable::rhandsontable(tbl, useTypes = FALSE, readOnly = T) %>% 
              rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            
            new_action_log_record(log_path, "File info", paste0("File '", file_name,"' uploaded"))
            file_focus(file_name)
          }, error = function(e){
            shiny::showNotification("Upload failed. Please consult the log for more information.", type = "error")
            new_action_log_record(log_path, "File error", paste0("Upload of file '", file_name,"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
          }
        )
      })
      
      # Update log
      action_log(read_action_log(log_path))
    })
    
    #### File browser ####
    output$file_browser = renderUI({
      req(user_data)
      ui = tagList(
        fluidRow(
          class = "file-grid",
          column(
            12,
            lapply(names(user_data), function(file_name){
              if(is.null(user_data[[file_name]])){
                return()
              }
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
    
    #### File editor ####
    output$file_editor = renderUI({
      req(file_focus())
      ui = div(
        id = ns(paste0("editor_", file_focus())),
        fluidRow(
          column(width = 12,
                 actionButton(ns("edit"), "Edit", width = "80px"),
                 actionButton(ns("transpose"), "Transpose", width = "120px"),
                 actionButton(ns("delete"), "Delete", width = "80px")
          )
        ),
        rhandsontable::rHandsontableOutput(ns("hot_table"))
      )
    })
    
    observe({
      req(file_focus(), user_data[[file_focus()]])
      output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])    
    })
    
    ##### Edit #####
    observeEvent(
      eventExpr = input$edit,
      handlerExpr = {
        insertUI(selector = paste0("#", ns("edit")),
                 where = "afterEnd",
                 ui = tagList(
                   actionButton(ns("save_edits"), "Save edits", class = "btn-success",  icon("check")),
                   actionButton(ns("discard_edits"), "Discard edits", class = "btn-danger", icon = icon("times")),
                 )
        )
        removeUI(selector = paste0("#", ns("edit")))
        
        user_data[[file_focus()]] = user_data[[file_focus()]] %>% 
          rhandsontable::hot_cols(readOnly = F)
      })
    
    ##### Save edits #####
    observeEvent(eventExpr = input$save_edits,
                 handlerExpr = {
                   tryCatch({
                     # Read edits
                     data_edited = input$hot_table
                     data_edited_df = rhandsontable::hot_to_r(data_edited)
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = rhandsontable::rhandsontable(data_edited_df, useTypes = FALSE, readOnly = T)
                     output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Restore UI state
                     insertUI(selector = paste0("#", ns("save_edits")),
                              where = "beforeBegin",
                              ui = actionButton(ns("edit"), "Edit"))
                     removeUI(selector = paste0("#", ns("save_edits")))
                     removeUI(selector = paste0("#", ns("discard_edits")))
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Saved user modifications for file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   })
                 })
    
    
    
    ##### Discard edits #####
    observeEvent(eventExpr = input$discard_edits,
                 handlerExpr = {
                   # Restore UI state
                   insertUI(selector = paste0("#", ns("save_edits")),
                            where = "beforeBegin",
                            ui = actionButton(ns("edit"), "Edit"))
                   removeUI(selector = paste0("#", ns("save_edits")))
                   removeUI(selector = paste0("#", ns("discard_edits")))
                   
                   user_data[[file_focus()]]$x$contextMenu = F
                   for(i in 1:length(user_data[[file_focus()]]$x$columns)){
                     user_data[[file_focus()]]$x$columns[[i]]$readOnly = T
                   }
                 })
    
    ##### Transpose table #####
    observeEvent(eventExpr = input$transpose,
                 handlerExpr = {
                   data_df = rhandsontable::hot_to_r(input$hot_table)
                   data_df_tr = data.frame(t(data_df))
                   
                   # Overwrite user data
                   user_data[[file_focus()]] = rhandsontable::rhandsontable(data_df_tr, useTypes = FALSE, readOnly = T)
                   output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                   
                   # Update action log 
                   shiny::showNotification("File transposed")
                   new_action_log_record(log_path, "File info", paste0("File '", file_focus(),"' transposed"))
                   action_log(read_action_log(log_path))
                 })
    
    ##### Delete file #####
    observeEvent(eventExpr = input$delete,
                 handlerExpr = {
                   # Remove user data and file focus
                   file_name = isolate(file_focus())
                   user_data[[file_name]] = NULL
                   output$hot_table = NULL
                   file_focus(NULL)
                   
                   # Update action log 
                   shiny::showNotification("File deleted")
                   new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted"))
                   action_log(read_action_log(log_path))
                 })
    
    # Return reactive user data
    return(user_data)
  })
}
