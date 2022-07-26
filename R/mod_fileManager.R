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
             tagList(
               tags$label("Upload a file"),
               tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                      title = "Supported data formats: 
                      \nTabular data: .csv, .txt, .tsv, .xls and .xlsx 
                      \nTurboveg data: .xml 
                      \nSee 'About > Tutorial' for more information."),
               fileInput(ns("upload"), label = NULL, width = "100%", multiple = T, placeholder = "Select a file", accept = c(".csv", ".txt", ".tsv", ".tab", "xls", "xlsx", ".xml"))
             )
    ),
    mainPanel(
      width = 10, 
      fluidRow(
        column(
          10, 
          offset = 1,
          tagList(       
            tags$label("Uploaded files"),
            tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                   title = "These files are available for further operations. Pick a file for edits in the File Editor"),
            uiOutput(ns("file_browser")),
            tags$hr(),
            div(
              tags$label("File Editor"),
              tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                     title = "Review and and edit files before importing to VegX"),
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
      if(any(input$upload$name %in% names(user_data))){
        existing_files = input$upload$name[which(input$upload$name %in% names(user_data))]
        showModal(
          modalDialog(paste0("The following existing files will be replaced by the your upload: ", paste0(existing_files, collapse = ", ")), footer = NULL, easyClose = T)
        )
      }
      
      lapply(input$upload$name, function(file_name){     
        tryCatch(
          expr = {
            file_info = input$upload[which(input$upload$name == file_name),]
            file_ext = tools::file_ext(file_name)
            if(file_ext == "csv"){
              user_data[[file_name]] = utils::read.csv(file_info$datapath, fileEncoding="UTF-8") %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext %in% c("tab", "tsv", "txt")){
              user_data[[file_name]]  = utils::read.delim(file_info$datapath, fileEncoding="UTF-8") %>% 
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
            new_action_log_record(log_path, "File info", paste0("File '", file_name,"' uploaded."))
          }, error = function(e){
            shiny::showNotification("Upload failed. Please consult the log for more information.", type = "error")
            new_action_log_record(log_path, "File error", paste0("Upload of file '", file_name,"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
          }, finally = {
            shinyjs::reset("upload")
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
                          "txt" = icon("file", "fa-9x black"),
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
        div(
          id = ns(paste0("editor_", file_focus())),
          fluidRow(
            column(width = 12,
                   actionButton(ns("edit"), "Edit", width = "130px", class = "btn-xs"),
                   actionButton(ns("delete"), "Delete selected file", width = "130px", class = "btn-xs")
            )
          ),
          aceEditor(outputId = ns("editor"), value = as.character(user_data[[file_focus()]]), height = "500px", mode = "xml", theme = "tomorrow", readOnly = T, autoComplete = "disabled")
        )
      } else {
        div(
          id = ns(paste0("editor_", file_focus())),
          fluidRow(
            column(width = 12,
                   actionButton(ns("edit"), "Edit", width = "130px", class = "btn-xs"),
                   actionButton(ns("reshape"), "Reshape", width = "130px", class = "btn-xs"),
                   actionButton(ns("delete"), "Delete selected file", width = "130px", class = "btn-xs")
            )
          ),
          rhandsontable::rHandsontableOutput(ns("editor"), height = 500)
        )
      }
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
                   actionButton(ns("save_edits"), "Save edits",  width = "130px", class = "btn-success btn-xs",  icon("check")),
                   actionButton(ns("discard_edits"), "Discard edits", width = "130px", class = "btn-danger btn-xs", icon = icon("times")),
                 )
        )
        removeUI(selector = paste0("#", ns("edit")))
        removeUI(selector = paste0("#", ns("reshape")))
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
        new_action_log_record(log_path, "File info", paste0("Entered edit mode for file '", file_focus(),"'."))
        action_log(read_action_log(log_path))
      })
    
    ##### Save edits #####
    observeEvent(eventExpr = input$save_edits,
                 handlerExpr = {
                   tryCatch({
                     file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                     if(file_ext == "xml"){
                       user_data[[file_focus()]] = read_xml(isolate(input$editor))
                       
                       insertUI(selector = paste0("#", ns("save_edits")),
                                where = "beforeBegin",
                                ui = actionButton(ns("edit"), "Edit", width = "130px", class = "btn-xs"))
                       insertUI(selector = paste0("#", ns("save_edits")),
                                where = "afterEnd",
                                ui = actionButton(ns("delete"), "Delete selected file", width = "130px", class = "btn-xs"))
                       removeUI(selector = paste0("#", ns("save_edits")))
                       removeUI(selector = paste0("#", ns("discard_edits")))
                     } else {
                       # Read edits
                       data_edited = input$editor
                       data_edited_df = rhandsontable::hot_to_r(data_edited)
                       
                       # Overwrite user data
                       user_data[[file_focus()]] = rhandsontable::rhandsontable(data_edited_df, useTypes = FALSE, readOnly = T) %>% 
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       
                       output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                       
                       # Restore UI state
                       insertUI(selector = paste0("#", ns("save_edits")), 
                                where = "beforeBegin",
                                ui = actionButton(ns("edit"), "Edit", width = "130px", class = "btn-xs"))
                       insertUI(selector = paste0("#", ns("save_edits")), 
                                where = "beforeBegin",
                                ui = actionButton(ns("reshape"), "Reshape", width = "130px", class = "btn-xs"))
                       insertUI(selector = paste0("#", ns("save_edits")),
                                where = "afterEnd",
                                ui = actionButton(ns("delete"), "Delete selected file", width = "130px", class = "btn-xs"))
                       
                       removeUI(selector = paste0("#", ns("save_edits")))
                       removeUI(selector = paste0("#", ns("discard_edits")))
                     }
                     
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
                   file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                   
                   # Restore data
                   user_data[[file_focus()]] = data_unedited()
                   
                   if(file_ext == "xml"){
                     updateAceEditor(session, "editor", value = as.character(user_data[[file_focus()]]), readOnly = T)
                   }
                   
                   # Restore UI state
                   insertUI(selector = paste0("#", ns("save_edits")), 
                            where = "beforeBegin",
                            ui = actionButton(ns("edit"), "Edit", width = "130px", class = "btn-xs"))
                   if(file_ext != "xml"){
                     insertUI(selector = paste0("#", ns("save_edits")), 
                              where = "beforeBegin",
                              ui = actionButton(ns("reshape"), "Reshape", width = "130px", class = "btn-xs"))
                   }
                   insertUI(selector = paste0("#", ns("save_edits")),
                            where = "afterEnd",
                            ui = actionButton(ns("delete"), "Delete selected file", width = "130px", class = "btn-xs"))
                   
                   removeUI(selector = paste0("#", ns("save_edits")))
                   removeUI(selector = paste0("#", ns("discard_edits")))
                   
                   # Update action log
                   new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were discarded."))
                   action_log(read_action_log(log_path))
                 })
    
    ##### Reshape table #####
    observeEvent(eventExpr = input$reshape,
                 handlerExpr = {
                   select_choices = unlist(input$editor$params$rColnames)
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Reshape data"),
                         tags$p("Prepare your datasets for import by organizing variables in columns and observations in rows. The primary use case is to reshape multiple columns that 
                                 code for the same variables, e.g. 'cover_tree_layer', 'cover_shrub_layer' and 'cover_herb_layer', into a tidy dataset where information on
                                 layer and measurements are cleanly separated.", class = "text-info"),
                         tags$div(style = "text-align: center; margin-bottom: 8px;",
                                  tags$img(src='www/images/reshape_table.png', align = "center", width = "100%")
                         ),
                         tags$p("This operation will create a new file in the File Manager. Note that VegXshiny requires a date and plot id column for all observation datasets. Mark the 
                                 corresponding columns of the original dataset to be retained during the operation.", class = "text-info"),
                         
                         hr(),
                         tags$label("Column selection"),
                         tags$p("Which columns ..."),
                         fluidRow(
                           column(6, tags$p("...are ", tags$span("id columns", style = "color: #1976d2; font-weight: bold"), "? *"),
                                  selectizeInput(ns("columns_id"), label = NULL, choices = select_choices, multiple = T, width = "100%")),
                           column(6, tags$p("...are ", tags$span("value columns", style = "color: #c62828; font-weight: bold"), "? *"),
                                  selectizeInput(ns("columns_pivot"), label = NULL, choices = select_choices, multiple = T, width = "100%"))
                         ),
                         
                         tags$label("Column names"),
                         tags$p("What should be the name of the column that holds the ..."),
                         fluidRow(
                           column(6, tags$p("...", tags$span("names of the value columns", style = "color: #66bb6a; font-weight: bold"), "? *"),
                                  textInput(ns("names_to"), label = NULL, width = "100%")),
                           column(6,  tags$p("...", tags$span("values of the value columns", style = "color: #651fff; font-weight: bold"), "? *"),
                                  textInput(ns("values_to"), label = NULL, width = "100%"))
                         ),
                         
                         tags$label("Empty values"),
                         fluidRow(
                           column(12,
                                  tags$p("Are empty values coded with a special string (e.g. NA, 0, etc.)?"),
                                  textInput(ns("na_string"), label = NULL, width = "25%")
                           )
                         ),
                         
                         tags$label("Label names"),
                         tags$p("Should parts of the column names be removed?"),
                         fluidRow(
                           column(6, tags$p("Remove prefix:"),
                                  textInput(ns("prefix_remove"), label = NULL, width = "100%")),
                           column(6, tags$p("Remove suffix:"),
                                  textInput(ns("suffix_remove"), label = NULL, width = "100%"))
                         ),
                         
                         tags$label("Dataset name"),
                         tags$p("Name of the new dataset without file extension *"),
                         textInput(ns("new_file_name"), label = NULL, width = "100%")
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_reshape"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_reshape,
                 handlerExpr = {
                   tryCatch({
                     inputs_present = all(sapply(c(input$columns_id, input$columns_pivot, input$names_to, input$values_to, input$new_file_name), isTruthy))
                     if(!inputs_present){
                       shiny::showNotification("Please fill out all mandatory fields.", type = "error")
                       return()
                     }
                     
                     data_df = rhandsontable::hot_to_r(input$editor) %>% 
                       dplyr::select(c(input$columns_id, input$columns_pivot)) %>% 
                       tidyr::pivot_longer(cols = input$columns_pivot,
                                           names_to = input$names_to,
                                           names_pattern = paste0(input$prefix_remove, "(.*)", input$suffix_remove),
                                           values_to = input$values_to) %>% 
                       mutate(!!input$values_to := na_if(.[[input$values_to]], input$na_string)) %>% 
                       tidyr::drop_na(!!input$values_to)
                       
                       # Update user data
                       file_name = paste0(input$new_file_name, ".csv")
                       user_data[[file_name]] = data_df %>% 
                         rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       
                       # Update action log 
                       removeModal()
                       new_action_log_record(log_path, "File info", paste0("Created reshaped table"))
                       action_log(read_action_log(log_path))
                       shiny::showNotification("Reshaped table created")
                   }, error = function(e){
                     removeModal()
                     new_action_log_record(log_path, "File error", paste0("Reshaping operating for ", file_focus(), " failed with the follwing exceptions:", 
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                   })
                 })
    
    observeEvent(eventExpr = input$dismiss_modal, 
                 handlerExpr = removeModal())
    
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
                   new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted."))
                   action_log(read_action_log(log_path))
                 })
    
    # Return reactive user data
    return(user_data)
  })
}
