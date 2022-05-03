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
      fluidRow(
        column(
          10, 
          offset = 1,
          tagList(
            tags$label("File browser"),
            uiOutput(ns("file_browser")),
            tags$hr(),
            div(
              tags$label("File Editor"),
              uiOutput(ns("file_editor")),
              style = "margin-bottom: 20px"
            )
          )  
        )
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
    data_unedited = reactiveVal()
    
    
    # TODO Import TV2/TV3
    #### Upload ####
    # Process and render uploaded files
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
            # Loop over file names in user_data()
            lapply(names(user_data), function(file_name){
              if(is.null(user_data[[file_name]])){
                return()
              }
              # Assign appropriate file icon
              file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
              icon = switch(file_ext,
                            "csv" = icon("file-csv", "fa-9x black"),
                            "xls" = icon("file-excel", "fa-9x black"),
                            "xlsx" = icon("file-excel", "fa-9x black"),
                            icon("file-spreadsheet", "fa-9x black"))
              
              # Create Button and Event listener
              file_button = actionButton(ns(paste0("button_", file_name)),
                                         width = "180px", height = "180px", class = "btn-success no-margin",
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
                 actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"),
                 actionButton(ns("delete"), "Delete", width = "80px", class = "btn-xs")
          )
        ),
        rhandsontable::rHandsontableOutput(ns("hot_table"), height = 500)
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
        # Modify UI
        insertUI(selector = paste0("#", ns("edit")),
                 where = "beforeBegin",
                 ui = tagList(
                   actionButton(ns("save_edits"), "Save edits",  width = "120px", class = "btn-success btn-xs",  icon("check")),
                   actionButton(ns("transpose"), "Transpose", width = "120px", class = "btn-info btn-xs"),
                   actionButton(ns("add_names"), "Add col-names", width = "120px", class = "btn-info btn-xs"),
                   actionButton(ns("discard_edits"), "Discard edits", width = "120px", class = "btn-danger btn-xs", icon = icon("times")),
                 )
        )
        removeUI(selector = paste0("#", ns("edit")))
        removeUI(selector = paste0("#", ns("delete")))
        
        # Save current state of user data
        data_unedited(user_data[[file_focus()]])
        
        # Make user_data editable
        user_data[[file_focus()]] = user_data[[file_focus()]] %>% 
          rhandsontable::hot_cols(readOnly = F)
        
        # Update action log
        new_action_log_record(log_path, "File info", paste0("Entered edit mode for file '", file_focus(),"'"))
        action_log(read_action_log(log_path))
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
                              ui = actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"))
                     insertUI(selector = paste0("#", ns("save_edits")),
                              where = "beforeBegin",
                              ui = actionButton(ns("delete"), "Delete", width = "80px", class = "btn-xs"))
                     removeUI(selector = paste0("#", ns("save_edits")))
                     removeUI(selector = paste0("#", ns("transpose")))
                     removeUI(selector = paste0("#", ns("add_names")))
                     removeUI(selector = paste0("#", ns("discard_edits")))
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Left edit mode for file for file '", file_focus(),"'. All edits were saved."))
                     action_log(read_action_log(log_path))
                   })
                 })
    
    
    
    ##### Discard edits #####
    observeEvent(eventExpr = input$discard_edits,
                 handlerExpr = {
                   # Restore UI state
                   insertUI(selector = paste0("#", ns("save_edits")),
                            where = "beforeBegin",
                            ui = actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"))
                   insertUI(selector = paste0("#", ns("save_edits")),
                            where = "beforeBegin",
                            ui = actionButton(ns("delete"), "Delete", width = "80px", class = "btn-xs"))
                   removeUI(selector = paste0("#", ns("save_edits")))
                   removeUI(selector = paste0("#", ns("transpose")))
                   removeUI(selector = paste0("#", ns("add_names")))
                   removeUI(selector = paste0("#", ns("discard_edits")))
                   
                   # Restore data
                   user_data[[file_focus()]] = data_unedited()
                   
                   # Update action log
                   new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were discarded"))
                   action_log(read_action_log(log_path))
                 })
    
    ##### Transpose table #####
    observeEvent(eventExpr = input$transpose,
                 handlerExpr = {
                   # Transpose data
                   data_df = rhandsontable::hot_to_r(input$hot_table)
                   data_df_tr = data.frame(t(data_df))
                   
                   # Overwrite user data
                   user_data[[file_focus()]] = rhandsontable::rhandsontable(data_df_tr)
                   output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                   
                   # Update action log 
                   shiny::showNotification("File transposed")
                   new_action_log_record(log_path, "File info", paste0("File '", file_focus(),"' transposed"))
                   action_log(read_action_log(log_path))
                 })
    
    ##### Add colnames #####
    ###### Modal dialogue #####
    observeEvent(eventExpr = input$add_names,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       selectInput(ns("name_source"), label = "Create column names from row", user_data[[file_focus()]]$x$rowHeaders),
                       tags$label("Names:"),
                       renderText(paste(rhandsontable::hot_to_r(input$hot_table)[input$name_source,], collapse = ", ")),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                   actionButton(ns("confirm_add_names"), class = "pull-right btn-success", "Confirm", icon("check")))
                       ),
                     )
                   )
                 })
    
    ###### Confirm  #####
    observeEvent(eventExpr = input$confirm_add_names, 
                 handlerExpr = {
                   tryCatch({
                     # Modify data
                     data_df = rhandsontable::hot_to_r(input$hot_table)
                     if(nrow(data_df) <= 1){
                       shiny::showNotification("Can't remove last row.", type = "warning")
                       return()
                     }
                     if(length(as.character(data_df[input$name_source,])) != length(unique(as.character(data_df[input$name_source,])))){
                       shiny::showNotification("Column names must be unique", type = "warning")
                       return()
                     }
                     
                     colnames(data_df) = data_df[input$name_source,]
                     data_df = data_df[rownames(data_df) != input$name_source,]
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = rhandsontable::rhandsontable(data_df)
                     output$hot_table = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log 
                     shiny::showNotification("Names added")
                     new_action_log_record(log_path, "File info", paste0("Column names added to file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File edit error", paste0("Adding colnames to file '", file_focus(),"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     removeModal()  
                   })
                 })
    
    ###### Dismiss modal  #####
    observeEvent(eventExpr = input$dismiss_modal, 
                 handlerExpr = {
                   removeModal()
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
