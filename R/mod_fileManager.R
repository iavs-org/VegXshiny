#' fileManager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom rhandsontable renderRHandsontable

mod_fileManager_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    tabsetPanel(
      tabPanel("Data handling",
         tagList(
           tags$head(
             tags$style(
               ".tab-content  {
                padding-left: 20px;
                padding-right: 20px;
               }"
             )
           ),
         tags$h1("Upload and prepare your data"),
         fluidRow(
           column(
             12,
             tags$label("Upload a file"),
             tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                    title = "Supported data formats: 
             \nTabular data: .csv, .txt, .tsv, .xls and .xlsx 
             \nTurboveg data: .xml 
             \nSee 'About > Tutorial' for more information."),
             fileInput(ns("upload"), label = NULL, width = "100%", placeholder = "", multiple = T, accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx", ".xml")),
             tags$hr(),
             tags$label("Uploaded files"),
             tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                    title = "These files are available for further cloud operations (import to Veg-X, edit); they can not be downloaded."),
             fluidRow(
               class = "file-grid",
               column(
                 12,
                 uiOutput(ns("file_browser"))
               )
             ),
             tags$hr(),
             div(
               tags$label("File Editor"),
               tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                      title = "Click on a file above to open the editor"),
               
               uiOutput(ns("edit_toolbar")),
               
               uiOutput(ns("file_viewer")),
               style = "margin-bottom: 20px"
             )
           )
         )
        )
      ),
      tabPanel("Help",
        div(
          class = "content",
          tags$h1("Upload and prepare your data"),
          tags$p("This is the single entry point for user-supplied 
                  data. ", tags$span("All files that contain information intended 
                  for import need to be uploaded here. ", class = "text-info"), 
                 "To upload files, click the upload widget on the left panel 
                  and browse your local file system. You can select and upload 
                  multiple files at once by pressing [CTRL]. Currently, the 
                  following file types are supported: "),
          tags$h4("Tabular data:"),
          tags$ul(
            tags$li(".csv (comma-separated)"),
            tags$li(".txt, .tsv (tab-separated)"),
            tags$li(".xls, .xslx")
          ),
          tags$h4("Turboveg 2:"),
          tags$ul(tags$li("xml")),
          tags$h4("Veg-X:"),
          tags$ul(tags$li("xml")),
          tags$p("If an upload of tabular data fails, check the following 
                  possible reasons or fixes:"),
          tags$ul(
            tags$li("Column names are expected in the first line and their 
                    number must match the number of columns."),
            tags$li("If you have a text file and commas as column separators,
                    use '.csv' as file extension. In case of tabs
                    use '.tsv' or '.txt'."), 
            tags$li("Space is currently not recognized as a delimiter.")
          ),
          tags$p("Completed uploads are listed under 'Uploaded Files' with a 
                  corresponding icon for the file type."),
          tags$p("Clicking on one of the file icons under 'Uploaded files' 
                  will activate the 'File Editor'. Depending on the file type,
                  this may either be a text editor (for xml data) or a 
                  spreadsheet editor (for tabular data). Click on the 'Edit' 
                  tab above the data view to access functions for editing and
                  reshaping the uploaded files."), 
                  
          tags$h1("Editing tools"),
            div(class = "info-box",
              tags$p("Regarding tabular data:"),
              div(class = "text-info info-box-item",
                  icon("lightbulb", class = "icon-padded-right"),
                  tags$span(style = "font-size:1.8rem;", "Importing observations 
                    (e.g. coverage values per species and plot) requires the 
                    data to be in 'long' format. Use the 'pivot' function to 
                    transform your observation data before import. Records are 
                    identified by plot id or, in case of time-dependent 
                    observatons, by plot id and date. Make sure your observation 
                    data contains the respective columns.")),
              div(class = "text-info info-box-item",
                  icon("lightbulb", class = "icon-padded-right"),
                  tags$span(style = "font-size:1.8rem;", "Feeling lost? We are 
                    preparing video tutorials for common use cases and will link 
                    to them here as they become available."))
          ),
          
          tagList(
            tags$p("Basic table editing 
                  can be reached using the context menu (right click
                  on a column or row name when the table is in editing 
                  mode). The options are self-explanatory."), 
            tags$p("There is currently a bug in the underlying packages that 
                   prevents the table from being saved after removing scattered 
                   columns. You can work around this by first saving the object 
                   under a new name and then removing individual columns or 
                   blocks of columns. Save the object after each removal."),
            tags$p("More tools are available via the buttons at the top:"),      
            div(class = "info-box",
                ## Save edits
                div(class = "info-box-item",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1em", "Save edits")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", 
                      "Save: Save edits overwriting the current object")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", 
                      "Save as: Save edits as a new object")),
                
                ## Reshape table
                div(class = "info-box-item",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1em", "Reshape table")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Pivot: Transform table from 'wide' to 
                  'long' format. Opens an input screen with detailed instructions")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Transpose: Turn rows into columns")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Crop: Removal of table sections")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Merge columns: Merge two columns into one. 
                              This can be needed when data points are identified by more than one column. The 
                              merged column is appended as the last column.")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Split column: Reverse the merging of
                              columns")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Row to 
                              colnames: Create column names from a row")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Column 
                              to rownames: Create rownames from a column")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Rownames 
                              to column: Transform rownames into a new column")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Colnames 
                              to row: Transform column names into a new row")),
                
                ## Edit values
                div(class = "info-box-item",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1em;", "Edit 
                              values")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Edit 
                              colnames: Beautify column names")),
                div(class = "info-box-item", style = "margin-left: 30px;",
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1.8rem; color: black;", "Format 
                              date: For import, dates must be listed in a 
                              column. If they are not, transpose the table (and 
                              back-transpose if necessary). Veg-X requires dates 
                              to be in the international ISO 8601 format 
                              (YYYY-MM-DD, e.g. 2024-05-30)."),
                    tags$span(style = "font-size:1.8rem; color: black;", "You 
                              need to specify what type of date you have to 
                              start the conversion. For example, if the dates 
                              are numbers generated during an Excel import from 
                              a Windows machine, enter the string 'excel-win' 
                              (without quotation marks) in the 'Original format' 
                              field. The string for MAC Excel is 'excel-mac'. 
                              Other definitions follow R conventions. For 
                              example, if your values look like 17.12.2023, the 
                              full conversion specification is %d.%m.%Y. For 
                              02/21/18 it is %m/%d/%y. Common symbols are %d 
                              (day of month as number), %m (month as number), 
                              %b (month abbreviated like Jan), %B (month full 
                              name like January), %y (year 2 digit), %Y (year 
                              4 digit). The separators are used literally (like 
                              ' ' for space). See", 
                        tags$a("the R documentation", href = "https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime", target = "_blank"),
                              " for more information. If a valid original format 
                              has been entered, a conversion example will appear 
                              at the bottom.")
                 ),
                     
                                        
                ## Discard edits
                div(class = "info-box-item", style = "margin-top: 10px;", 
                    icon("caret-right", class = "icon-padded-right"),
                    tags$span(style = "font-size:1em; color:black;", "Discard
                              edits: Exit the edit mode without saving")
                )
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
mod_fileManager_server <- function(id, file_order, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    upload_focus = reactiveVal()       # Keeps track of current file in upload loop
    upload_queue = reactiveVal()       # Used for async reactive chains in file upload 
    xlsx_focus = reactiveVal()         # excel file, for which modal is shown
    user_data = reactiveValues()       # Uploaded data
    data_unedited = reactiveVal()      # Copy of unedited file
    observer_list = reactiveVal()      # Tracks for which button in file browser observers have been created already
    file_focus = reactiveVal()         # Currently selected file in File browser
    edit_mode = reactiveVal(F)         # Tracks whether the file editor is in editor mode
    
    #### General Observers ####
    observeEvent(eventExpr = input$dismiss_modal,
                 handlerExpr = removeModal())
    
    # ------------------------------------------------------------------------ #
    #### Upload ####
    upload_file = function(file_name, file_info){
      file_ext = tools::file_ext(file_name)
      if(file_ext == "csv"){
        file_data = utils::read.csv(file_info$datapath, fileEncoding="UTF-8") %>% 
          rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T, 
                                       outsideClickDeselects = TRUE) %>% 
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      } else if(file_ext %in% c("tab", "tsv", "txt")){
        file_data = utils::read.delim(file_info$datapath, fileEncoding="UTF-8") %>% 
          rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T,
                                       outsideClickDeselects = TRUE) %>% 
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      } else if(file_ext == "xml"){
        file_data = xml2::read_xml(file_info$datapath)
      } else if(file_ext %in% c("xls", "xlsx")){
        excel_sheets_available = readxl::excel_sheets(file_info$datapath)
        xlsx_focus(file_name)
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
      } else (
        stop("Unsupported file format.")
      )
      
      if(exists("file_data")){
        queue_data(file_name, file_data)
      }
    }
    
    queue_data = function(file_name, file_data){
      upload_focus(file_name)
      if(!is.null(file_order()) && file_name %in% file_order()){
        showModal(
          modalDialog(
            paste0("Should the existing file (", file_name,") be replaced?"), 
            size = "m",
            title = file_name,
            footer = tagList(
              tags$span(actionButton(ns("skip_file"), "Skip upload", class = "pull-left"), 
                        actionButton(ns("upload_file"), "Overwrite existing file", class = "pull-right "))
            )
          )
        )
        upload_queue_item = list(
          upload_confirmed = FALSE,
          file_name = file_name,
          file_data = file_data
        )
      } else {
        upload_queue_item = list(
          upload_confirmed = TRUE,
          file_name = file_name,
          file_data = file_data
        )
      }
      queue = isolate(upload_queue())
      queue[[file_name]] = upload_queue_item
      upload_queue(queue)
    }
    
    ###### Observe input$upload #####
    observeEvent(eventExpr = input$upload, 
                 handlerExpr = { 
                   file_names = input$upload$name
                   lapply(file_names, function(file_name){    
                     tryCatch(
                       expr = {
                         file_info = input$upload[which(input$upload$name == file_name),]
                         upload_file(file_name, file_info)
                       }, error = function(e){
                         shiny::showNotification("Upload failed. Please consult the log for more information.", type = "error")
                         new_action_log_record(log_path, "File error", paste0("Upload of file '", file_name,"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                       }  
                     )
                   })
                   
                   # Reset upload 
                   shinyjs::reset("upload")
                   
                   # Update log
                   action_log(read_action_log(log_path))
                 })
    
    
    ###### Observe upload queue #####
    observeEvent(eventExpr = upload_queue(),
                 handlerExpr = {
                   req(upload_queue)
                   queue_items = lapply(upload_queue(), function(upload_queue_item){
                     if(upload_queue_item$upload_confirmed){
                       user_data[[upload_queue_item$file_name]] = upload_queue_item$file_data
                       file_order_cleaned = setdiff(file_order(), upload_queue_item$file_name)
                       file_order(c(file_order_cleaned, upload_queue_item$file_name))
                       new_action_log_record(log_path, "File info", paste0("File '", upload_queue_item$file_name,"' uploaded."))
                       return(NULL)
                     } else {
                       return(upload_queue_item)
                     }
                   })
                   
                   unconfirmed_queue_items = queue_items[lengths(queue_items) != 0]
                   if(length(unconfirmed_queue_items) == 0) {
                     upload_queue(NULL)
                   } else {
                     upload_queue(unconfirmed_queue_items)
                   }
                   
                   action_log(read_action_log(log_path))
                 })
    
    ###### Observe confirm/skip upload button #####
    observeEvent(eventExpr = input$skip_file, 
                 handlerExpr = {
                   removeModal() 
                 })
    
    observeEvent(eventExpr = input$upload_file, 
                 handlerExpr = {
                   queue = isolate(upload_queue())
                   queue[[upload_focus()]]["upload_confirmed"] = TRUE
                   upload_queue(queue)
                   removeModal() 
                 })
    
    ###### Confirm Excel Import #####
    observeEvent(eventExpr = input$confirm_read_excel, 
                 handlerExpr = {
                   req(user_data, input$upload$name, input$excel_sheets_selected)
                   
                   tryCatch({
                     file_name = isolate(xlsx_focus())
                     file_ext = tools::file_ext(file_name)
                     file_info = input$upload[which(input$upload$name == file_name),]
                     
                     for(sheet in input$excel_sheets_selected){
                       sheet_name = paste0(tools::file_path_sans_ext(file_name), "_", sheet, ".", file_ext)
                       # Update user_data
                       sheet_data = readxl::read_excel(file_info$datapath, sheet = sheet) %>% 
                         rhandsontable::rhandsontable(useTypes = FALSE, readOnly = T, selectCallback = TRUE,
                                                      outsideClickDeselects = TRUE) %>% 
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       
                       queue_data(sheet_name, sheet_data)
                     }
                   }, error = function(e){
                     shiny::showNotification("Import failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File error", paste0("File import from ", input$upload$name, " failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     xlsx_focus(NULL)
                     removeModal()  
                   })
                 })
    
    # ------------------------------------------------------------------------ #
    #### File browser ####
    create_file_buttons = function(){
      req(file_order)
      button_list = lapply(file_order(), function(file_name){ # Loop over file names
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
          title = file_name,
          actionButton(ns(paste0("select_", file_name)),
                       width = "180px", height = "180px", 
                       class = button_class,
                       label = div(class = "overlay-button-container-label", icon, tags$br(), file_name, `data-tooltip` = file_name)),
          actionButton(inputId = ns(paste0("delete_", file_name)),
                       class = "btn-delete btn-xs", label = "",
                       icon = icon("times"))
        )
      })
      return(button_list)
    }
    
    create_file_button_observers = function(){
      file_names = setdiff(file_order(), observer_list())
      lapply(file_names, function(file_name){
        # Click Event
        observeEvent(
          ignoreInit = F,
          eventExpr = input[[paste0("select_", file_name)]],
          handlerExpr = {
            if(edit_mode() == T){
              shiny::showNotification("File is still in edit mode. Please save or discard your changes first.", type = "warning")
            } else {
              file_focus(file_name)  
            }
          }
        )
        
        # Delete event
        observeEvent(
          ignoreInit = T,
          eventExpr = input[[paste0("delete_", file_name)]],
          handlerExpr = {
            if(edit_mode() == T){
              shiny::showNotification("File is still in edit mode. Please save or discard your changes first.", type = "warning")
            } else {
              # Remove data
              .subset2(user_data, "impl")$.values$remove(file_name) # hacky way to remove item from reactiveValues
              
              # Remove file focus
              if(!is.null(file_focus()) && file_name == file_focus()){
                file_focus(NULL)
                output$editor = NULL
                output$file_viewer = NULL
              }
              
              # Update file_order
              file_order(setdiff(file_order(), file_name))
              
              # Update action log
              shiny::showNotification(paste0(file_name, " deleted"))
              new_action_log_record(log_path, "File info", paste0("File '", file_name,"' deleted."))
              action_log(read_action_log(log_path))
            }
          }
        )
        
        # Add to observer list
        observer_list(c(observer_list(), file_name))
      })
    }
    
    observeEvent(eventExpr = file_order(),
                 handlerExpr = {
                   output$file_browser = renderUI({
                     file_buttons = create_file_buttons()
                     create_file_button_observers()
                     return(file_buttons)
                   })
                 })
    
    # ------------------------------------------------------------------------ #
    #### File editor ####
    observeEvent(eventExpr = list(edit_mode(), file_focus()),
                 handlerExpr = {
                   if(is.null(file_focus())){
                     output$edit_toolbar = renderUI(div())
                     return()
                   } 
                   
                   if(edit_mode() == T){
                     file_ext = tools::file_ext(file_focus())
                     
                     output$edit_toolbar = renderUI({
                       button_list = list(
                         save = shinyWidgets::dropdownButton(
                           inputId = "save_dropdown",
                           label = "Save edits",
                           size = "xs",
                           circle = FALSE,
                           inline = TRUE,
                           actionButton(ns("save"), "Save",  width = "110px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("save_as"), "Save as",  width = "110px", class = "btn-xs btn-dropdown-item")
                         ),
                         reshape = shinyWidgets::dropdownButton(
                           inputId = "reshape_dropdown",
                           label = "Reshape table",
                           size = "xs",
                           circle = FALSE,
                           inline = TRUE,
                           actionButton(ns("pivot"), "Pivot", width = "135px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("transpose"), "Transpose", width = "135px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("crop"), "Crop", width = "135px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("merge_columns"), "Merge columns", width = "135px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("split_column"), "Split column", width = "135px", class = "btn-xs btn-dropdown-item"),
                           actionButton(ns("row_to_colnames"), "Row to colnames",  width = "135px", class = "btn-xs btn-dropdown-item rounded-0"),
                           actionButton(ns("col_to_rownames"), "Column to rownames",  width = "135px", class = "btn-xs btn-dropdown-item rounded-0"),
                           actionButton(ns("rownames_to_col"), "Rownames to column",  width = "135px", class = "btn-xs btn-dropdown-item rounded-0"),
                           actionButton(ns("colnames_to_row"), "Colnames to row",  width = "135px", class = "btn-xs btn-dropdown-item rounded-0")
                         ),
                         edit_names = shinyWidgets::dropdownButton(
                           inputId = "names_dropdown",
                           label = "Edit values",
                           size = "xs",
                           circle = FALSE,
                           inline = TRUE,
                           actionButton(ns("edit_colnames"), "Edit colnames",  width = "130px", class = "btn-xs btn-dropdown-item rounded-0"),
                           actionButton(ns("format_date"), "Format date", width = "130px", class = "btn-xs btn-dropdown-item")
                         ),
                         discard = actionButton(ns("discard"), "Discard edits", width = "130px", class = "btn-xs", icon = icon("times"))
                       )
                       
                       if(file_ext == "xml"){   # remove table-specific buttons
                         button_list[["reshape"]] = NULL  
                         button_list[["edit_names"]] = NULL
                       }
                       
                       return(tagList(button_list))
                     })
                     
                     # Save current state of user data
                     data_unedited(user_data[[file_focus()]])
                     
                     # Make user_data editable
                     if(file_ext == "xml"){
                       updateAceEditor(session, "editor", readOnly = F)
                     } else {
                       user_data[[file_focus()]] = rhandsontable::hot_to_r(input$editor) %>%
                         rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     }
                   } else {
                     output$edit_toolbar = renderUI({ 
                       actionButton(ns("edit_file"), "Edit", width = "130px", class = "btn-xs")
                     })
                   }
                 })
    
    observeEvent(eventExpr = file_focus(),
                 handlerExpr = {
                   file_ext = tools::file_ext(file_focus())
                   if(file_ext == "xml"){
                     output$file_viewer = renderUI(aceEditor(outputId = ns("editor"), value = as.character(user_data[[file_focus()]]), 
                                                             height = "500px", mode = "xml", theme = "tomorrow", readOnly = T, autoComplete = "disabled"))
                   } else {
                     output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     output$file_viewer = renderUI(rhandsontable::rHandsontableOutput(ns("editor"), height = 500))
                   } 
                 })
    
    observeEvent(
      eventExpr = input$edit_file,
      handlerExpr = {
        edit_mode(T)
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
                     file_ext = tools::file_ext(file_focus())
                     if(file_ext == "xml"){
                       user_data[[file_focus()]] = read_xml(isolate(input$editor))
                     } else {
                       # Read edits
                       data_edited_df = rhandsontable::hot_to_r(input$editor)
                       
                       # Overwrite user data
                       user_data[[file_focus()]] = data_edited_df %>% 
                         rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T, 
                                                      outsideClickDeselects = TRUE) %>%
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       
                       output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     }
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were saved."))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     new_action_log_record(log_path, "File error", paste0("Saving file ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                     user_data[[file_focus()]] = data_unedited()
                   }, finally = {
                     # Restore UI state
                     removeModal()
                     edit_mode(F)
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
                     file_ext = tools::file_ext(file_focus())
                     file_name_orig = file_focus()
                     if(file_ext == "xml"){
                       file_name = paste0(input$new_file_name, ".", file_ext)
                       data_edited = read_xml(isolate(input$editor))
                       user_data[[file_name]] = data_edited
                     } else {
                       file_name = paste0(input$new_file_name, ".", file_ext)
                       data_edited_df = rhandsontable::hot_to_r(input$editor)
                       
                       # Reset current file
                       user_data[[file_focus()]] = data_unedited()
                       
                       # Write new file
                       user_data[[file_name]] = data_edited_df %>% 
                         rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, readOnly = T,
                                                      outsideClickDeselects = TRUE) %>%
                         rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                     }
                     
                     # Update file browser
                     file_focus(file_name)
                     file_order(c(file_order(), file_name))
                     
                     # Update action log
                     shiny::showNotification("Edits saved")
                     new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_name_orig,"'. All edits were saved to ", file_name, "."))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     new_action_log_record(log_path, "File error", paste0("Saving file ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                     user_data[[file_focus()]] = data_unedited()
                   }, finally = {
                     # Restore UI state
                     removeModal()
                     edit_mode(F)
                   })
                 })
    
    ##### Reshape ####
    ###### Pivot #####
    observeEvent(eventExpr = input$pivot,
                 handlerExpr = {
                   select_choices = unlist(input$editor$params$rColnames)
                   showModal(
                     ## shinyjqui::draggableModalDialog would be useful but the 
                     ## dropdown does not work with that function
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h1("Pivot data"),
                         tags$p(icon("lightbulb", class = "icon-padded-right"),
                                "The Veg-X import requires a transformation of 
                                'wide tables' with columns that code for the 
                                same measured variable (e.g. multiple species 
                                columns with cover values) to be transformed 
                                into 'long tables' with one column for such 
                                measurements.", class = "text-info"),
                         
                          tags$div(class = "panel-group", id = "accordion", 
                            
                            # --- Example 1 ---
                            tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading",
                                tags$h4(class = "panel-title",
                                  tags$a("Pivoting with two ID columns", 
                                         `data-toggle` = "collapse", 
                                         `data-parent` = "#accordion", 
                                         href = paste0("#", ns("collapse1")))
                                )
                              ),
                              tags$div(id = ns("collapse1"), class = "panel-collapse collapse",
                                tags$div(class = "panel-body",
                                  "The required index columns (plot ID, or plot 
                                  ID and date) must be available as columns 
                                  before pivoting. This often requires some 
                                  preparatory work (see tutorial videos).",
                                  tags$div(style = "text-align: left",
                                  tags$img(src='www/images/just_pivot_species.png', 
                                          align = "center", width = 540)),
                                  tags$p("In this example, the value 0 is 
                                  removed during pivoting. In a last step before
                                  importing to Veg-X, the taxon column would be 
                                  splitted into taxa and layers (Reshape table > 
                                  Split column)."),
                                )
                              )
                            ),
                          # ------------
                         
                          # --- Example 2 ---
                            tags$div(class = "panel panel-default",
                              tags$div(class = "panel-heading",
                                tags$h4(class = "panel-title",
                                  tags$a("Additional removal of prefixes", 
                                         `data-toggle` = "collapse", 
                                         `data-parent` = "#accordion", 
                                         href = paste0("#", ns("collapse2")))
                                )
                              ),
                              tags$div(id = ns("collapse2"), class = "panel-collapse collapse",
                                tags$div(class = "panel-body",
                                  "Prefixes (such as 'Cover_' in this example) 
                                  and suffixes can be removed during pivoting.",
                                tags$div(style = "text-align: left",
                                 tags$img(src='www/images/just_pivot_layers.png', 
                                          align = "center", width = 540)),
                                )
                              )
                            )
                          # ------------
                          ),                           
                                                  

                         hr(),
                         tags$label("Column selection"),
                         tags$p(icon("lightbulb", class = "icon-padded-right"),
                                "Use the matching colors in the images above 
                                (follow the links) and of the labels below as 
                                visual clues.", class = "text-info"),
                         tags$p("Select id columns from the drop-down menu and 
                                remove any other unneeded value columns from the 
                                right-hand box (you do not need to remove id 
                                columns here as they are ignored)."),
                         fluidRow(
                           column(6, tags$p(tags$span("ID Columns", style = "color: #0072b2; font-weight: bold"), "*"),
                                  selectizeInput(ns("columns_id"), label = NULL, choices = select_choices, multiple = T, width = "100%")),
                           column(6, tags$p(tags$span("Value Columns", style = "color: #c62828; font-weight: bold"), "*"),
                                  selectizeInput(ns("columns_pivot"), label = NULL, 
                                                 choices = select_choices, 
                                                 selected = setdiff(unlist(input$editor$params$rColnames), input$columns_id),
                                                 multiple = T, width = "100%"))
                         ),
                         
                         tags$label("Column names"),
                         tags$p("What should be the name of the ..."),
                         fluidRow(
                           column(6, tags$p("... ", tags$span("column that names what has been measured", style = "color: #009e73; font-weight: bold"), "? *"),
                                  textInput(ns("names_to"), label = NULL, width = "100%")),
                           column(6, tags$p("... ", tags$span("column with the measurements", style = "color: #cc79a7; font-weight: bold"), "? *"),
                                  textInput(ns("values_to"), label = NULL, width = "100%")),
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
                         )
                       ),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_pivot"), class = "pull-right btn-success", "Confirm", icon("check")))
                       )
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$columns_id,
                 handlerExpr = {
                   updateSelectizeInput(session,
                                        inputId = ns("columns_pivot"), 
                                        choices = setdiff(unlist(input$editor$params$rColnames), input$columns_id),
                                        selected = setdiff(input$columns_pivot, input$columns_id),
                                        server = FALSE)
                 })
    
    
    
    observeEvent(eventExpr = input$confirm_pivot,
                 handlerExpr = {
                   tryCatch({
                     inputs_present = all(sapply(c(input$columns_id, input$columns_pivot, input$names_to, input$values_to), isTruthy))
                     if(!inputs_present){
                       shiny::showNotification("Please fill out all mandatory fields.", type = "error")
                       return()
                     }

                     columns_pivot = setdiff(input$columns_pivot, input$columns_id) # Ignore columns used as id cols
                     data_df = rhandsontable::hot_to_r(input$editor) %>%
                       dplyr::select(c(input$columns_id, columns_pivot)) %>%
                       tidyr::pivot_longer(cols = columns_pivot,
                                           names_to = input$names_to,
                                           names_pattern = paste0(input$prefix_remove, "(.*)", input$suffix_remove),
                                           values_to = input$values_to) %>%
                       mutate(!!input$values_to := as.character(.[[input$values_to]])) %>% 
                       mutate(!!input$values_to := na_if(.[[input$values_to]], input$na_string)) %>%
                       tidyr::drop_na(!!input$values_to)
                     
                     # Update user data
                     user_data[[file_focus()]] = data_df %>%
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE) 
                     
                     new_action_log_record(log_path, "File info", paste0("File '", file_focus(),"' pivoted"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Table pivoted")
                   }, error = function(e){
                     new_action_log_record(log_path, "File error", paste0("Reshaping operating for ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                   }, finally = {
                     removeModal()
                   })
                 })
    
    ###### Transpose #####
    observeEvent(eventExpr = input$transpose,
                 handlerExpr = {
                   # Transpose data
                   data_df = rhandsontable::hot_to_r(input$editor)
                   data_df_tr = data.frame(t(data_df))
                   
                   # Overwrite user data
                   user_data[[file_focus()]] = data_df_tr %>% 
                     rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                   output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                   
                   # Update action log
                   shiny::showNotification("Table transposed")
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
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     
                     # Update action log
                     new_action_log_record(log_path, "File info", paste0("Cropped table to selection (",
                                                                         "rows: ", row_min, "-", row_max , ", ",
                                                                         "cols: ", col_min, "-", col_max,  
                                                                         ") in  file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     new_action_log_record(log_path, "File error", paste0("Extraction from ", file_focus(), " failed with the following exceptions:",
                                                                          "<ul><li>Error: ", e$message, "</li></ul>"))
                     action_log(read_action_log(log_path))
                     shiny::showNotification("Operation failed. Please consult the log for more information.", type = "error")
                   }, finally = {
                     removeModal()
                   })
                 })
    
    ###### Merge columns #####
    observeEvent(eventExpr = input$merge_columns,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       tags$h3("Merge columns"),
                       tags$p("Merge two columns into one. This may be needed when processing data where each data 
                              point is identified by more than one row and column, e.g. coverage values per species and stratum at different
                              plots and dates. Use the 'split column' function to later expand a column again.", class = "text-info"),
                       selectizeInput(ns("merge_colname_1"), label = "Column 1", user_data[[file_focus()]]$x$colHeaders),
                       selectizeInput(ns("merge_colname_2"), label = "Column 2", user_data[[file_focus()]]$x$colHeaders),
                       selectizeInput(ns("merge_separator"), 
                                      label = "Separator",
                                      choices = c("|", "_",";", ",", "/", "~"), 
                                      selected = "|", 
                                      options=list(create=TRUE)),
                       textInput(ns("new_colname"), label = "New column name"),
                       checkboxInput(ns("remove_merged_columns"), label = "Remove original columns from data", value = T, width = "100%"),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_merge_columns"), class = "pull-right btn-success", "Confirm", icon("check")))
                       ),
                     )
                   )
                 })
    
    
    observeEvent(eventExpr = input$confirm_merge_columns,
                 handlerExpr = {
                   tryCatch({
                     # Modify data
                     data_df <- rhandsontable::hot_to_r(input$editor) %>% 
                       dplyr::mutate(!!input$new_colname := paste(.[[input$merge_colname_1]], .[[input$merge_colname_2]], sep  = input$merge_separator))
                     
                     if(input$remove_merged_columns){
                       data_df = data_df %>% 
                         dplyr::select(-c(input$merge_colname_1, input$merge_colname_2))
                     }
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = data_df %>% 
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log
                     shiny::showNotification(paste0("Columns ", input$merge_colname_1, " and ", input$merge_colname_2, " merged"))
                     new_action_log_record(log_path, "File info", paste0("Columns ", input$merge_colname_1, " and ", input$merge_colname_2, " merged in file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File edit error", paste0("Merging columns in file '", file_focus(),"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     removeModal()
                   })
                 })
    
    ###### Split columns #####
    observeEvent(eventExpr = input$split_column,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       tags$h3("Split column"),
                       tags$p("Split a column into two.", class = "text-info"),
                       selectizeInput(ns("split_colname"), label = "Column", user_data[[file_focus()]]$x$colHeaders),
                       selectizeInput(ns("split_separator"), 
                                      label = "Separator",
                                      choices = c("|", "_", ";", ",", "/", "~"), 
                                      selected = "|", 
                                      options=list(create=TRUE)),
                       textInput(ns("new_colname_1"), label = "New column name 1"),
                       textInput(ns("new_colname_2"), label = "New column name 2"),
                       checkboxInput(ns("remove_split_column"), label = "Remove original column from data", value = T, width = "100%"),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_split_column"), class = "pull-right btn-success", "Confirm", icon("check")))
                       ),
                     )
                   )
                 })
    
    
    observeEvent(eventExpr = input$confirm_split_column,
                 handlerExpr = {
                   tryCatch({
                     # Modify data
                     data_df <- rhandsontable::hot_to_r(input$editor) %>% 
                       tidyr::separate(col = input$split_colname, 
                                       into = c(input$new_colname_1, input$new_colname_2),
                                       sep = stringr::str_escape(input$split_separator), 
                                       remove = input$remove_split_column)
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = data_df %>% 
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log
                     shiny::showNotification(paste0("Column ", input$split_column, " split"))
                     new_action_log_record(log_path, "File info", paste0("Column ", input$split_column, " split in file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File edit error", paste0("Splitting column in file '", file_focus(),"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     removeModal()
                   })
                 })
    
    
    ##### Edit data #####
    ###### Edit column names ####
    observeEvent(eventExpr = input$edit_colnames,
                 handlerExpr = {
                   # prepare colnames df
                   colnames_df = data.frame(as.list(input$editor$params$rColnames))
                   colnames_hot = rhandsontable::rhandsontable(colnames_df, 
                                                               colHeaders = NULL, 
                                                               rowHeaders = NULL, 
                                                               useTypes = FALSE,
                                                               rowHeights = 30, 
                                                               selectCallback = T)
                   output$colnames_hot = rhandsontable::renderRHandsontable(colnames_hot)
                   showModal(
                     modalDialog(
                       tags$label("New column names"),
                       rhandsontable::rHandsontableOutput(ns("colnames_hot")),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_edit_colnames"), class = "pull-right btn-success", "Confirm", icon("check")))
                       ),
                     )
                   )
                 })
    
    
    observeEvent(eventExpr = input$confirm_edit_colnames,
                 handlerExpr = {
                   tryCatch({
                     # Modify data
                     data_df = rhandsontable::hot_to_r(input$editor)
                     new_colnames = rhandsontable::hot_to_r(input$colnames_hot)
                     
                     colnames(data_df) = new_colnames[1,]
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = data_df %>% 
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log
                     shiny::showNotification("Column names edited")
                     new_action_log_record(log_path, "File info", paste0("Column names edited in file '", file_focus(),"'"))
                     action_log(read_action_log(log_path))
                   }, error = function(e){
                     shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File edit error", paste0("Editing column names in file '", file_focus(),"' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, finally = {
                     removeModal()
                   })
                 })
    
    ###### Row to colnames ####
    observeEvent(eventExpr = input$row_to_colnames,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       selectInput(ns("source_row"), label = "Create column names from row", user_data[[file_focus()]]$x$rowHeaders),
                       tags$label("Names:"),
                       renderText(paste(rhandsontable::hot_to_r(input$editor)[input$source_row,], collapse = ", ")),
                       footer = tagList(
                         tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                                   actionButton(ns("confirm_row_to_colnames"), class = "pull-right btn-success", "Confirm", icon("check")))
                       ),
                     )
                   )
                 })
    
    
    observeEvent(eventExpr = input$confirm_row_to_colnames,
                 handlerExpr = {
                   tryCatch({
                     # Modify data
                     data_df = rhandsontable::hot_to_r(input$editor)
                     new_colnames = as.character(data_df[input$source_row,])
                     
                     if(nrow(data_df) <= 1){
                       shiny::showNotification("Can't remove last row.", type = "warning")
                       return()
                     }
                     if(length(new_colnames) != length(unique(new_colnames))){
                       shiny::showNotification("Column names must be unique", type = "warning")
                       return()
                     }
                     if(!all(sapply(new_colnames, isTruthy))){
                       shiny::showNotification("Column names may not be empty", type = "warning")
                       return()
                     }
                     
                     colnames(data_df) = new_colnames
                     data_df = data_df[rownames(data_df) != input$source_row,]
                     
                     # Overwrite user data
                     user_data[[file_focus()]] = data_df %>% 
                       rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
                     output$editor = rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log
                     shiny::showNotification("Column names added")
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
    
    ###### Column to rownames ####
    observeEvent(input$col_to_rownames, {
      showModal(
        modalDialog(
          selectInput(ns("source_column"), label = "Create rownames from column", user_data[[file_focus()]]$x$colHeaders),
          footer = tagList(
            tags$span(
              actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
              actionButton(ns("confirm_col_to_rownames"), "Confirm", class = "pull-right btn-success", icon("check"))
            )
          )
        )
      )
    })
    
    observeEvent(input$confirm_col_to_rownames, {
      tryCatch({
        # Modify data
        data_df <- rhandsontable::hot_to_r(input$editor) %>% 
          tibble::column_to_rownames(input$source_column)
        
        # Overwrite user data
        user_data[[file_focus()]] <- data_df %>%
          rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
        output$editor <- rhandsontable::renderRHandsontable(user_data[[file_focus()]])
        
        # Update action log
        shiny::showNotification("Rownames added")
        new_action_log_record(log_path, "File info", paste0("New rownames created from column ", input$source_column  ,"  in file '", file_focus(), "'"))
        action_log(read_action_log(log_path))
      }, error = function(e) {
        shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
        new_action_log_record(log_path, "File edit error", paste0("Adding rownames to file '", file_focus(), "' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
        action_log(read_action_log(log_path))
      }, finally = {
        removeModal()
      })
    })
    
    ###### Rownames to column ####
    observeEvent(input$rownames_to_col, {
      showModal(
        modalDialog(
          textInput(ns("new_column"), label = "Name of the new column"),
          footer = tagList(
            tags$span(
              actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
              actionButton(ns("confirm_rownames_to_col"), "Confirm", class = "pull-right btn-success", icon("check"))
            )
          )
        )
      )
    })
    
    observeEvent(input$confirm_rownames_to_col, {
      tryCatch({
        if(!isTruthy(input$new_column)){
          shiny::showNotification("Please provide a name for the new column", type = "warning")
          return()
        }
        
        # Modify data
        data_df <- rhandsontable::hot_to_r(input$editor) %>% 
          tibble::rownames_to_column(input$new_column)
        
        # Overwrite user data
        user_data[[file_focus()]] <- data_df %>%
          rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
        output$editor <- rhandsontable::renderRHandsontable(user_data[[file_focus()]])
        
        # Update action log
        shiny::showNotification("Column added")
        new_action_log_record(log_path, "File info", paste0("Column ", input$new_column, " added in file '", file_focus(), "'"))
        action_log(read_action_log(log_path))
      }, error = function(e) {
        shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
        new_action_log_record(log_path, "File edit error", paste0("Adding new column to file '", file_focus(), "' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
        action_log(read_action_log(log_path))
      }, finally = {
        removeModal()
      })
    })
    
    ###### Colnames to row ####
    observeEvent(input$colnames_to_row, {
      showModal(
        modalDialog(
          textInput(ns("new_row"), label = "Name of the new row"),
          footer = tagList(
            tags$span(
              actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
              actionButton(ns("confirm_colnames_to_row"), "Confirm", class = "pull-right btn-success", icon("check"))
            )
          )
        )
      )
    })
    
    observeEvent(input$confirm_colnames_to_row, {
      tryCatch({
        if(!isTruthy(input$new_row)){
          shiny::showNotification("Please provide a name for the new row", type = "warning")
          return()
        }
        
        # Modify data
        data_df <- rhandsontable::hot_to_r(input$editor)
        new_row <- data.frame(matrix(names(data_df), nrow = 1), stringsAsFactors = FALSE)
        colnames(new_row) <- colnames(data_df)
        data_df <- rbind(new_row, data_df)

        # Overwrite user data
        user_data[[file_focus()]] <- data_df %>%
          rhandsontable::rhandsontable(useTypes = FALSE, selectCallback = TRUE, outsideClickDeselects = FALSE)
        output$editor <- rhandsontable::renderRHandsontable(user_data[[file_focus()]])
        
        # Update action log
        shiny::showNotification("Row added")
        new_action_log_record(log_path, "File info", paste0("Row ", input$new_row, " added in file '", file_focus(), "'"))
        action_log(read_action_log(log_path))
      }, error = function(e) {
        shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
        new_action_log_record(log_path, "File edit error", paste0("Adding new row to file '", file_focus(), "' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
        action_log(read_action_log(log_path))
      }, finally = {
        removeModal()
      })
    })
    
    ###### Format date #####
    date_valid = reactiveVal(F)
    
    output$date_input = renderText({
        
      req(input$conversion_spec)

      dates <- rhandsontable::hot_to_r(input$editor) %>% 
        select(input$date_column) %>% 
        pull() %>% 
        .[. != ""] %>% 
        na.omit()
    
      valid_dates <- !is.na(as.Date(dates, format = input$conversion_spec))
      valid_numbers <- grepl("^\\d*\\.?\\d*$", dates)
      first_valid_value <- dates[valid_dates | valid_numbers][1]   
      as.character(first_valid_value)
    })
    
    observe({
      if (!is.null(input$conversion_spec) && (
          tolower(input$conversion_spec) == 'excel-win' ||
          tolower(input$conversion_spec) == 'excel-mac')) {
        date_valid(T)
      }
    })
    
    output$date_output = renderText({
      req(input$conversion_spec)
      tryCatch({
          
        # Extract a sample
          date_sample <- rhandsontable::hot_to_r(input$editor) %>% 
            select(input$date_column) %>% 
            pull() %>% 
            .[. != ""] %>% 
            na.omit()

          valid_dates <- !is.na(as.Date(date_sample, format = input$conversion_spec))
          valid_numbers <- grepl("^\\d*\\.?\\d*$", date_sample)
          first_valid_value <- date_sample[valid_dates | valid_numbers][1]
          date_sample <- as.character(first_valid_value)
        
          if (tolower(input$conversion_spec) == 'excel-win') {
            orig <- "1899-12-30"
          }  
          if (tolower(input$conversion_spec) == 'excel-mac') {
            orig <- "1904-01-01"
          }  
          if (tolower(input$conversion_spec) == 'excel-win' || 
              tolower(input$conversion_spec) == 'excel-mac') {
            
            # Convert Excel date if the value looks like numeric
            if (grepl("^\\d*\\.?\\d*$", date_sample)) {
              excel_date = as.numeric(date_sample)
            } else {
              return("Values that do not look like numbers will be skipped.")
            }  
            date_output = format(as.Date(excel_date, origin = orig), "%Y-%m-%d")
          
          } else {
          # Convert date using input$conversion_spec
            date_input = as.Date(date_sample, input$conversion_spec)
            date_output = format(date_input, "%Y-%m-%d")
          }
          if(is.na(date_output)){
            date_valid(F)
            return("Invalid original format")
          } else {
            date_valid(T)
            return(date_output)
          }
        })  
      })
    
    observeEvent(eventExpr = input$format_date,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       tags$h1("Reformat dates prior to import"),
                        selectInput(ns("date_column"), label = "Date column", 
                                    user_data[[file_focus()]]$x$colHeaders, 
                                    selected = NULL),
                        tags$div(style = "display: flex; padding-top: 20px;", 
                          tags$label("Original format"),
                          tags$i(class = "glyphicon glyphicon-info-sign icon-info text-info", 
                            title = "Example strings: 
                            \nExcel Windows: excel-win 
                            \nExcel Mac: excel-mac 
                            \n21.02.2021: %d.%m.%Y
                            \n02/21/18: %m/%d/%y
                            \n2021-02-21: %Y-%d-%m
                            \nFeb 2, 2023: %b %d, %Y
                            \nSee 'Help' for more possibilities.")
                        ),
                        div(class = "info-box",
                            div(class = "text-info info-box-item",
                                icon("lightbulb", class = "icon-padded-right"),
                                tags$span(style = "font-size:1.8rem;", "For 
                                  Excel imports use excel-win or excel-mac 
                                  depending on the originating system")
                            ),    
                            div(class = "text-info info-box-item",
                                icon("lightbulb", class = "icon-padded-right"),
                                tags$span(style = "font-size:1.8rem;", "For
                                  other formats follow R conventions (see
                                  Help and tooltips)")
                            )    
                        ),
                      textInput(ns("conversion_spec"), label = NULL),
                      fluidRow(
                          column(3,
                            tags$label("Date input"),
                            textOutput(ns("date_input"))
                          ),
                          column(3,
                            tags$label("Date output"),
                            textOutput(ns("date_output"))
                          )
                      ),
                      footer = tagList(
                         tags$span(
                           actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                           shinyjs::disabled(actionButton(ns("confirm_format_date"), "Confirm", class = "pull-right btn-success", icon("check")))
                        )
                      )
                    )
                  )
                })
    
    
    observeEvent(eventExpr = date_valid(),
                 handlerExpr = {
                   if (date_valid() || !is.null(input$conversion_spec) && 
                       (tolower(input$conversion_spec) == 'excel-win' || 
                        tolower(input$conversion_spec) == 'excel-mac')){
                     shinyjs::enable("confirm_format_date")
                   } else {
                     shinyjs::disable("confirm_format_date")
                   }
                 })
    
    observeEvent(eventExpr = input$confirm_format_date,
                 handlerExpr = {
                   tryCatch({

                     # Convert non-Excel dates
                     data_df <- rhandsontable::hot_to_r(input$editor) 
                     if (tolower(input$conversion_spec) != 'excel-win' && 
                         tolower(input$conversion_spec) != 'excel-mac') {
                         date_formatted = as.Date(data_df[[input$date_column]], input$conversion_spec)
                         data_df[[input$date_column]] = format(date_formatted, "%Y-%m-%d")
                     } else {
                     # Convert Excel dates
                        date_selected = data_df[[input$date_column]]
                        convertible <- which(sapply(date_selected, function(x) {
                          no_warning_as_numeric <- function(x) {
                            tryCatch(as.numeric(x), warning = function(w) return(NA))
                         }
                         !is.na(no_warning_as_numeric(x))
                        }))
                        excel_dates <- as.numeric(date_selected[convertible])
                       
                        if (tolower(input$conversion_spec) == 'excel-win') {
                          orig <- "1899-12-30"
                        }  
                        if (tolower(input$conversion_spec) == 'excel-mac') {
                          orig <- "1904-01-01"
                        }  
                        vegx_dates <- as.Date(excel_dates, origin = orig)
                        date_strings <- format(vegx_dates, "%Y-%m-%d")
                        data_df[[input$date_column]][convertible] <- date_strings
                        data_df[is.na(data_df)] <- NA
                     }                     
                     
                     # Overwrite user data
                     user_data[[file_focus()]] <- rhandsontable::rhandsontable(data_df, useTypes = FALSE, 
                       selectCallback = TRUE, outsideClickDeselects = FALSE)
                     output$editor <- rhandsontable::renderRHandsontable(user_data[[file_focus()]])
                     
                     # Update action log
                     shiny::showNotification("Date formatted")
                     new_action_log_record(log_path, "File info", paste0("Reformatted date in column ", input$date_column  ,"  in file '", file_focus(), "'"))
                     action_log(read_action_log(log_path))
                   }, 
                   error = function(e) {
                     shiny::showNotification("Action failed. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "File edit error", paste0("Date formatting in file '", file_focus(), "' failed with the following exceptions:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   }, 
                   finally = {
                     removeModal()
                   })
                 })
    
    ##### Discard edits ####
    observeEvent(eventExpr = input$discard,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       size = "l",
                       tagList(
                         tags$h3("Exit and discard"),
                         tags$p("All changes made since the last save will be 
                                lost.")
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
                   file_ext = tools::file_ext(file_focus())
                   
                   # Restore data
                   user_data[[file_focus()]] = data_unedited()
                   
                   if(file_ext == "xml"){
                     updateAceEditor(session, "editor", value = as.character(user_data[[file_focus()]]), readOnly = T)
                   }
                   
                   # Update action log
                   new_action_log_record(log_path, "File info", paste0("Left edit mode for file '", file_focus(),"'. All edits were discarded."))
                   action_log(read_action_log(log_path))
                   
                   # Restore UI state
                   removeModal()
                   edit_mode(F)
                 })
    

    # Module server returns reactive user data
    return(user_data)
  })
}