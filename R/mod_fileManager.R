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
               fileInput(ns("upload"), label = NULL, width = "100%", multiple = T, placeholder = "Select a file", accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx", ".xml")),
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

# TODO
# - When uploading multiple files, xls and xlsx import will fail
# - in the Browser, the toolbar UI will not show up
# - btn-focus class is not always added when file focus changes (e.g. when saving as new file)
# - file remains in edit mode when switching to another file, but edit_toolbar is gone
# - crop doesnt work after transpose (reason: input$editor_select is outdated)
# - too many notifications after deleting file
# - Redesign rendering of file browser

#' fileManager Server Functions
#'
#' @noRd 
mod_fileManager_server <- function(id, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    user_data = reactiveValues()
    file_focus = reactiveVal()
    data_unedited = reactiveVal()
    upload_order = reactiveVal()
    observer_list = reactiveVal()
    
    #### Upload ####
    # Process and render uploaded files
    observeEvent(input$upload, {
      if(any(input$upload$name %in% names(user_data))){
        existing_files = input$upload$name[which(input$upload$name %in% names(user_data))]
        showModal(
          modalDialog(paste0("The following existing files will be replaced by the your upload: ", paste0(existing_files, collapse = ", ")), footer = NULL, easyClose = T)
          # TODO creates second button in multi upload
        )
      }
      
      lapply(input$upload$name, function(file_name){     
        tryCatch(
          expr = {
            file_info = input$upload[which(input$upload$name == file_name),]
            file_ext = tools::file_ext(file_name)
            if(file_ext == "csv"){
              user_data[[file_name]] = utils::read.csv(file_info$datapath, fileEncoding="UTF-8") %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext %in% c("tab", "tsv", "txt")){
              user_data[[file_name]]  = utils::read.delim(file_info$datapath, fileEncoding="UTF-8") %>% 
                rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T) %>% 
                rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            } else if(file_ext %in% c("xls", "xlsx")){
              excel_sheets_available = readxl::excel_sheets(file_info$datapath)
              showModal(
                modalDialog(
                  selectizeInput(ns("excel_sheets_selected"), 
                                 label = "Which Excel sheets should be imported?", 
                                 choices = excel_sheets_available,
                                 selected = excel_sheets_available[1],
                                 multiple = T, 
                                 width = "100%"),
                  footer = tagList(
                    tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                              actionButton(ns("confirm_read_excel"), class = "pull-right btn-success", "Confirm", icon("check")))
                  ),
                )
              )
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
      
      # Update upload_order
      upload_order(c(upload_order(), input$upload$name))
      
      # Update log
      action_log(read_action_log(log_path))
    })
    
    ###### Confirm Excel Import #####
    observeEvent(eventExpr = input$confirm_read_excel, 
                 handlerExpr = {
                   req(user_data, input$upload$name, input$excel_sheets_selected)
                   
                   tryCatch({
                     file_name = input$upload$name
                     file_info = input$upload[which(input$upload$name == file_name),]
                     
                     for(sheet in input$excel_sheets_selected){
                       sheet_name = paste0(file_name, "_", sheet)
                       user_data[[paste0(sheet_name, ".xlsx")]] = readxl::read_excel(file_info$datapath, sheet = sheet) %>% 
                         rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T) %>% 
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       upload_order(c(upload_order(), file_name))
                     }
                   }, error = function(e){
                     shiny::showNotification("Import failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File error", paste0("File import from ", input$upload$name, " failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     removeModal()  
                   })
                 })
    
    #### File browser ####
    create_file_buttons = function(){
      req(upload_order)
      ui = fluidRow(
        class = "file-grid",
        column(
          12,
          lapply(upload_order(), function(file_name){           # Loop over file names
            if(is.null(user_data[[file_name]])){
              return()
            }
            file_ext = tools::file_ext(file_name)
            icon = switch(file_ext,                             # Assign appropriate file icon
                          "csv" = icon("file-csv", "fa-9x black"),
                          "txt" = icon("file", "fa-9x black"),
                          "xls" = icon("file-excel", "fa-9x black"),
                          "xlsx" = icon("file-excel", "fa-9x black"),
                          "xml" = icon("file-code", "fa-9x black"),
                          icon("file", "fa-9x black"))
            
            # Create Button
            button_class = if(!is.null(file_focus()) && file_name == file_focus()){
              "btn-success btn-overlay no-margin btn-focus"
            } else {
              "btn-success btn-overlay no-margin"
            }
            
            file_button = div(
              class = "overlay-button-container",
              actionButton(ns(paste0("select_", file_name)),
                           width = "180px", height = "180px", 
                           class = button_class,
                           label = div(icon, tags$br(), file_name)),
              actionButton(inputId = ns(paste0("delete_", file_name)),
                           class = "btn-delete btn-xs", label = "",
                           icon = icon("times"))
            )
          })
        )
      )
      return(ui)
    }
    
    create_file_button_observers = function(){
      file_names = setdiff(upload_order(), observer_list())
      lapply(file_names, function(file_name){
        # Click Event
        observeEvent(
          ignoreInit = F,
          eventExpr = input[[paste0("select_", file_name)]],
          handlerExpr = {
            shinyjs::removeClass(id = paste0("select_", file_focus()), class = "btn-focus")
            file_focus(file_name)
            shinyjs::addClass(id = paste0("select_", file_focus()), class = "btn-focus")
          }
        )
        
        # Delete event
        observeEvent(
          ignoreInit = T,
          eventExpr = input[[paste0("delete_", file_name)]],
          handlerExpr = {
            # Remove data
            .subset2(user_data, "impl")$.values$remove(file_name) # hacky way to remove item from reactiveValues
            
            # Remove file focus
            if(!is.null(file_focus()) && file_name == file_focus()){
              file_focus(NULL)
              output$editor = NULL
            }
            
            # Update upload_order
            upload_order(setdiff(upload_order(), file_name))
            
            # Update action log
            shiny::showNotification(paste0(file_name, " deleted"))
            new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted."))
            action_log(read_action_log(log_path))
          }
        )
        
        # Add to observer list
        observer_list(c(observer_list(), file_name))
      })
    }
    
    observeEvent(eventExpr = upload_order(),
                 handlerExpr = {
                   output$file_browser = renderUI({
                     file_buttons = create_file_buttons()
                     create_file_button_observers()
                     return(file_buttons)
                   })
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
                   actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs")
            )
          ),
          aceEditor(outputId = ns("editor"), value = as.character(user_data[[file_focus()]]), height = "500px", mode = "xml", theme = "tomorrow", readOnly = T, autoComplete = "disabled")
        )
      } else {
        div(
          id = ns(paste0("editor_", file_focus())),
          fluidRow(
            column(width = 12,
                   actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs")
            )
          ),
          rhandsontable::rHandsontableOutput(ns("editor"), height = 500)
        )
      }
    })
    
    ##### General Observers ####
    observe({
      req(file_focus(), user_data[[file_focus()]])
      file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
      
      if(file_ext %in% c("csv", "tab", "tsv", "txt", "xls", "xlsx")){
        output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
      } 
    })
    
    observeEvent(eventExpr = input$dismiss_modal,
                 handlerExpr = removeModal())
    
    ##### Edit #####
    observeEvent(
      eventExpr = input$edit_file,
      handlerExpr = {
        # Modify UI
        insertUI(selector = paste0("#", ns("edit_file")),
                 where = "beforeBegin",
                 ui = div(
                   id = ns("edit_toolbar"),
                   shinyWidgets::dropdownButton(
                     inputId = "save_dropdown",
                     label = "Save edits",
                     status = "success",
                     size = "xs",
                     width = "130px",
                     circle = FALSE,
                     inline = TRUE,
                     actionButton(ns("save"), "Save",  width = "130px", class = "btn-xs text-left"),
                     actionButton(ns("save_as"), "Save as",  width = "130px", class = "btn-xs text-left")
                   ),
                   shinyWidgets::dropdownButton(
                     inputId = "reshape_dropdown",
                     label = "Reshape table",
                     size = "xs",
                     width = "130px",
                     circle = FALSE,
                     inline = TRUE,
                     actionButton(ns("pivot"), "Pivot", width = "130px", class = "btn-xs text-left"),
                     actionButton(ns("transpose"), "Transpose", width = "130px", class = "btn-xs text-left"),
                     actionButton(ns("crop"), "Crop", width = "130px", class = "btn-xs text-left")
                   ),
                   shinyWidgets::dropdownButton(
                     inputId = "names_dropdown",
                     label = "Edit names",
                     size = "xs",
                     width = "130px",
                     circle = FALSE,
                     inline = TRUE,
                     actionButton(ns("row_to_colnames"), "Row to colnames",  width = "130px", class = "btn-xs text-left rounded-0"),
                     actionButton(ns("col_to_rownames"), "Column to rownames",  width = "130px", class = "btn-xs text-left rounded-0"),
                     actionButton(ns("rownames_to_col"), "Rownames to column",  width = "130px", class = "btn-xs text-left rounded-0"),
                     actionButton(ns("colnames_to_row"), "Colnames to row",  width = "130px", class = "btn-xs text-left rounded-0")
                   ),
                   actionButton(ns("discard"), "Discard edits", width = "130px", class = "btn-danger btn-xs", icon = icon("times"))
                 )
        )
        removeUI(selector = paste0("#", ns("edit_file")))
        removeUI(selector = paste0("#", ns("delete_file")))
        
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
    
    ##### Save ####
    ###### Save edits #####
    observeEvent(eventExpr = input$save,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Save edits"),
                         tags$p("This will overwrite the existing file")
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_save"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_save,
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
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Left edit mode for file for file '", file_focus(),"'. All edits were saved."))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Something went wrong", type = "error")
                   }, finally = {
                     # Restore UI state
                     removeModal()
                     insertUI(selector = paste0("#", ns("edit_toolbar")),
                              where = "beforeBegin",
                              ui = actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs"))
                     
                     removeUI(selector = paste0("#", ns("edit_toolbar")))
                   })
                 })
    
    ###### Save edits as #####
    observeEvent(eventExpr = input$save_as,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Save edits as new file"),
                         tags$label("Dataset name *"),
                         tags$p("Name of the new dataset (without file extension)"),
                         textInput(ns("new_file_name"), label = NULL, width = "100%")
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_save_as"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_save_as,
                 handlerExpr = {
                   req(user_data, input$new_file_name)
                   tryCatch({
                     file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                     if(file_ext == "xml"){
                       file_name = paste0(input$new_file_name, ".", file_ext)
                       data_edited = read_xml(isolate(input$editor))
                       user_data[[file_name]] = data_edited
                     } else {
                       file_name = paste0(input$new_file_name, ".", file_ext)
                       data_edited = input$editor
                       data_edited_df = rhandsontable::hot_to_r(data_edited)
                       
                       # Reset current file
                       user_data[[file_focus()]] = data_unedited()
                       
                       # Write new file
                       user_data[[file_name]] = rhandsontable::rhandsontable(data_edited_df, useTypes = FALSE, readOnly = T) %>%
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                     }
                     
                     # Update file browser
                     file_focus(file_name)
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Left edit mode for file for file '", file_focus(),"'. All edits were saved."))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Something went wrong", type = "error")
                   }, finally = {
                     # Restore UI state
                     removeModal()
                     insertUI(selector = paste0("#", ns("edit_toolbar")),
                              where = "beforeBegin",
                              ui = actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs"))
                     
                     removeUI(selector = paste0("#", ns("edit_toolbar")))
                   })
                 })
    
    ##### Reshape ####
    ###### Pivot #####
    observeEvent(eventExpr = input$pivot,
                 handlerExpr = {
                   select_choices = unlist(input$editor$params$rColnames)
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Pivot data"),
                         tags$p("Prepare your datasets for import by organizing variables in columns and observations in rows. The primary use case is to reshape columns that
                                 code for the same variables, e.g. 'cover_tree_layer', 'cover_shrub_layer' and 'cover_herb_layer', into a key and a value column,
                                e.g. 'layer_name' and 'cover_value'.", class = "text-info"),
                         tags$div(style = "text-align: center; margin-bottom: 8px;",
                                  tags$img(src='www/images/pivot_table.png', align = "center", width = "100%")
                         ),
                         tags$p("This operation will create a new file in the File Manager and can be repeated to derive multiple tables. Use the color codes in the above figure and
                                 the below description texts to map your inputs correctly. Note that VegXshiny requires a date and plot id column
                                 for all observation datasets, so make sure to mark the corresponding columns as id columns.", class = "text-info"),
                         
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
                                  tags$p("Should values with a special string (e.g. NA, 0, etc.) be removed? Enter string:"),
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
                                   actionButton(ns("confirm_pivot"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_pivot,
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
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T) %>%
                       rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                     
                     # Update action log
                     removeModal()
                     new_action_log_record(log_path, "File info", paste0("Created pivotd table"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Pivotd table created")
                   }, error = function(e){
                     removeModal()
                     new_action_log_record(log_path, "File error", paste0("Reshaping operating for ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                   })
                 })
    
    
    ###### Transpose #####
    observeEvent(eventExpr = input$transpose,
                 handlerExpr = {
                   # Transpose data
                   data_df = rhandsontable::hot_to_r(input$editor)
                   data_df_tr = data.frame(t(data_df))
                   
                   # Overwrite user data
                   user_data[[file_focus()]] = rhandsontable::rhandsontable(data_df_tr)
                   output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                   
                   # Update action log
                   shiny::showNotification("File transposed")
                   new_action_log_record(log_path, "File info", paste0("File '", file_focus(),"' transposed"))
                   action_log(read_action_log(log_path))
                 })
    
    ###### Crop #####
    observeEvent(eventExpr = input$crop,
                 handlerExpr = {
                   row_min = input$editor_select$select$r
                   row_max = input$editor_select$select$r2
                   col_min = input$editor_select$select$c
                   col_max = input$editor_select$select$c2
                   
                   if(is.null(row_min) | is.null(row_max) | is.null(col_min) | is.null(col_max)){
                     shiny::showNotification("No cells selected", type = "warning")
                     return()
                   } 
                   
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Crop dataset"),
                         tags$p("Crop the dataset to the current selection:", class = "text-info"),
                         tags$p(paste0("Rows: ", paste(unique(c(row_min, row_max)), collapse = "-"))),
                         tags$p(paste0("Columns: ", paste(unique(c(col_min, col_max)), collapse = "-")))
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_crop"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_crop,
                 handlerExpr = {
                   tryCatch({
                     row_min = input$editor_select$select$r
                     row_max = input$editor_select$select$r2
                     col_min = input$editor_select$select$c
                     col_max = input$editor_select$select$c2
                     
                     data_df = rhandsontable::hot_to_r(input$editor) %>%
                       dplyr::slice(row_min:row_max) %>%
                       dplyr::select(col_min:col_max)
                     
                     # Update user data
                     user_data[[file_focus()]] = data_df %>%
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = T, readOnly = T) %>%
                       rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                     
                     # Update action log
                     removeModal()
                     new_action_log_record(log_path, "File info", paste0("Created new table"))
                     action_log(read_action_log(log_path))
                     
                   }, error = function(e){
                     removeModal()
                     new_action_log_record(log_path, "File error", paste0("Extraction from ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                   })
                 })
    
    ##### Edit names #####
    # observeEvent(eventExpr = input$row_to_colnames,
    #              handlerExpr = {
    #                showModal(
    #                  modalDialog(
    #                    selectInput(ns("name_source"), label = "Create column names from row", user_data[[file_focus()]]$x$rowHeaders),
    #                    tags$label("Names:"),
    #                    renderText(paste(rhandsontable::hot_to_r(input$editor)[input$name_source,], collapse = ", ")),
    #                    footer = tagList(
    #                      tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
    #                                actionButton(ns("confirm_add_names"), class = "pull-right btn-success", "Confirm", icon("check")))
    #                    ),
    #                  )
    #                )
    #              })
    # 
    
    # observeEvent(eventExpr = input$confirm_row_to_colnames, 
    #              handlerExpr = {
    #                tryCatch({
    #                  # Modify data
    #                  data_df = rhandsontable::hot_to_r(input$editor)
    #                  if(nrow(data_df) <= 1){
    #                    shiny::showNotification("Can't remove last row.", type = "warning")
    #                    return()
    #                  }
    #                  if(length(as.character(data_df[input$name_source,])) != length(unique(as.character(data_df[input$name_source,])))){
    #                    shiny::showNotification("Column names must be unique", type = "warning")
    #                    return()
    #                  }
    #                  
    #                  colnames(data_df) = data_df[input$name_source,]
    #                  data_df = data_df[rownames(data_df) != input$name_source,]
    #                  
    #                  # Overwrite user data
    #                  user_data[[file_focus()]] = rhandsontable::rhandsontable(data_df)
    #                  output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
    #                  
    #                  # Update action log 
    #                  shiny::showNotification("Names added")
    #                  new_action_log_record(log_path, "File info", paste0("Column names added to file '", file_focus(),"'"))
    #                  action_log(read_action_log(log_path))
    #                }, error = function(e){
    #                  shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
    #                  new_action_log_record(log_path, "File edit error", paste0("Adding colnames to file '", file_focus(),"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
    #                  action_log(read_action_log(log_path))
    #                }, finally = {
    #                  removeModal()  
    #                })
    #              })
    # 
    # 
    
    ##### Discard edits ####
    observeEvent(eventExpr = input$discard,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Discard edits"),
                         tags$p("All edits will be lost.")
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_discard"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$confirm_discard,
                 handlerExpr = {
                   file_ext = stringr::str_split(file_focus(), "\\.", simplify = T)[-1]
                   
                   # Restore data
                   user_data[[file_focus()]] = data_unedited()
                   
                   if(file_ext == "xml"){
                     updateAceEditor(session, "editor", value = as.character(user_data[[file_focus()]]), readOnly = T)
                   }
                   
                   # Restore UI state
                   insertUI(selector = paste0("#", ns("edit_toolbar")),
                            where = "beforeBegin",
                            ui = actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs"))
                   removeUI(selector = paste0("#", ns("edit_toolbar")))
                   
                   # Update action log
                   new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were discarded."))
                   action_log(read_action_log(log_path))
                   
                   removeModal()
                 })
    
    # Return reactive user data
    return(user_data)
  })
}
