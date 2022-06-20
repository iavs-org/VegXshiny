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
             fileInput(ns("upload"), "Upload files", width = "100%", multiple = T, accept = c(".csv", ".txt", ".tsv", ".tab", "xls", "xlsx", ".xml"))
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
    
    #### Upload ####
    # Process and render uploaded files
    observeEvent(input$upload, {
      lapply(input$upload$name, function(file_name){     
        tryCatch(
          expr = {
            file_info = input$upload[which(input$upload$name == file_name),]
            file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
            if(file_ext == "csv"){
              user_data[[file_name]] = utils::read.csv(file_info$datapath) %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext %in% c("tab", "tsv", "txt")){
              user_data[[file_name]]  = utils::read.delim(file_info$datapath) %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext %in% c("xls", "xlsx")){
              user_data[[file_name]] = readxl::read_excel(file_info$datapath) %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext == "xml"){
              user_data[[file_name]] = xml2::read_xml(file_info$datapath)
            } else (
              stop("Unsupported file format.")
            )
  
            new_action_log_record(log_path, "File info", paste0("File '", file_name,"' uploaded"))
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
                          "txt" = icon("file-csv", "fa-9x black"),
                          "xls" = icon("file-excel", "fa-9x black"),
                          "xlsx" = icon("file-excel", "fa-9x black"),
                          "xml" = icon("file-code", "fa-9x black"),
                          icon("file", "fa-9x black"))
            
            file_button = actionButton(ns(paste0("button_", file_name)),              # Create Button and Event listener
                                       width = "180px", height = "180px", class = "btn-success no-margin",
                                       label = div(icon, tags$br(), file_name))
            
            observeEvent(
              ignoreInit = F,
              eventExpr = input[[paste0("button_", file_name)]],
              handlerExpr = {
                shinyjs::removeClass(id = paste0("button_", file_focus()), class = "btn-focus")
                file_focus(file_name)
                shinyjs::addClass(id = paste0("button_", file_focus()), class = "btn-focus")
              }         
            )
            return(file_button)
          })
        )
      )
      return(ui)
    }) 
    
    #### File editor ####
    output$file_editor = renderUI({
      req(file_focus())
      file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
      
      if(file_ext == "xml"){
        output_function = uiOutput
      } else {
        output_function = rhandsontable::rHandsontableOutput
      }
      ui = div(
        id = ns(paste0("editor_", file_focus())),
        fluidRow(
          column(width = 12,
                 actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"),
                 actionButton(ns("delete"), "Delete", width = "80px", class = "btn-xs")
          )
        ),
        if(file_ext == "xml"){
          aceEditor(outputId = ns("editor"), value = as.character(user_data[[file_focus()]]), height = "500px", mode = "xml", theme = "tomorrow", readOnly = T, autoComplete = "disabled")
        } else {
          rhandsontable::rHandsontableOutput(ns("editor"), height = 500)
        }
      )
    })
    
    observe({
      req(file_focus(), user_data[[file_focus()]])
      file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
      
      if(file_ext %in% c("csv", "tab", "tsv", "txt", "xls", "xlsx")){
        output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
      } 
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
                   actionButton(ns("discard_edits"), "Discard edits", width = "120px", class = "btn-danger btn-xs", icon = icon("times")),
                 )
        )
        removeUI(selector = paste0("#", ns("edit")))
        removeUI(selector = paste0("#", ns("delete")))
        
        # Save current state of user data
        data_unedited(user_data[[file_focus()]])
        
        # Make user_data editable
        file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
        if(file_ext == "xml"){
          updateAceEditor(session, "editor", readOnly = F)
        } else {
          user_data[[file_focus()]] = user_data[[file_focus()]] %>% 
            rhandsontable::hot_cols(readOnly = F) %>% 
            rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)  
        }
        
        # Update action log
        new_action_log_record(log_path, "File info", paste0("Entered edit mode for file '", file_focus(),"'"))
        action_log(read_action_log(log_path))
      })
    
    ##### Save edits #####
    observeEvent(eventExpr = input$save_edits,
                 handlerExpr = {
                   tryCatch({
                     file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                     if(file_ext == "xml"){
                       user_data[[file_focus()]] = read_xml(isolate(input$editor))
                     } else {
                       # Read edits
                       data_edited = input$editor
                       data_edited_df = rhandsontable::hot_to_r(data_edited)
                       
                       # Overwrite user data
                       user_data[[file_focus()]] = rhandsontable::rhandsontable(data_edited_df, useTypes = FALSE, readOnly = T) %>% 
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       
                       output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     }

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
                   }, error = function(e){
                     shiny::showNotification("Something went wrong", type = "error")
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
                   
                   file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                   if(file_ext == "xml"){
                     updateAceEditor(session, "editor", value = as.character(user_data[[file_focus()]]), readOnly = T)
                   }
                   
                   # Update action log
                   new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were discarded"))
                   action_log(read_action_log(log_path))
                 })
    
    ##### Delete file #####
    observeEvent(eventExpr = input$delete,
                 handlerExpr = {
                   # Remove user data and file focus
                   file_name = isolate(file_focus())
                   .subset2(user_data, "impl")$.values$remove(file_name) # hacky way to remove item from reactiveValues
                   output$editor = NULL
                   file_focus(NULL)
                   
                   # Redraw file browser
                   output$file_browser = renderUI({
                     req(user_data)
                     ui = fluidRow(
                       class = "file-grid",
                       column(
                         12,
                         # Loop over file names in user_data()
                         lapply(names(user_data), function(file_name){
                           # Assign appropriate file icon
                           file_ext = stringr::str_split(file_name, "\\.", simplify = T)[-1]
                           icon = switch(file_ext,                                       # Assign appropriate file icon
                                         "csv" = icon("file-csv", "fa-9x black"),
                                         "txt" = icon("file-csv", "fa-9x black"),
                                         "xls" = icon("file-excel", "fa-9x black"),
                                         "xlsx" = icon("file-excel", "fa-9x black"),
                                         "xml" = icon("file-code", "fa-9x black"),
                                         icon("file", "fa-9x black"))
                           
                           # Create Button and Event listener
                           file_button = actionButton(ns(paste0("button_", file_name)),
                                                      width = "180px", height = "180px", class = "btn-success no-margin",
                                                      label = div(icon, tags$br(), file_name))
                           
                           observeEvent(
                             ignoreInit = F,
                             eventExpr = input[[paste0("button_", file_name)]],
                             handlerExpr = {
                               shinyjs::removeClass(id = paste0("button_", file_focus()), class = "btn-focus")
                               file_focus(file_name)
                               shinyjs::addClass(id = paste0("button_", file_focus()), class = "btn-focus")
                             }         
                           )
                           return(file_button)
                         })
                       )
                     )
                     return(ui)
                   }) 
                   
                   # Update action log 
                   shiny::showNotification(paste0(file_name, " deleted"))
                   new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted"))
                   action_log(read_action_log(log_path))
                 })
    
    # Return reactive user data
    return(user_data)
  })
}
