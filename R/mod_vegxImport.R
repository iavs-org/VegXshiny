#' vegxImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vegxImport_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tabsetPanel(
      tabPanel("Read Veg-X",
        fluidRow(
          column(
            width = 12,
            tags$h1 ("Reading existing Veg-X XML"),
            fluidRow(
              column(6, selectizeInput(ns("vegx_file"), width = "100%", label = NULL, choices = c("No files found" = ""))),
              column(6, div(style = "display:left-align", actionButton(ns("read_vegx"), label = "Read VegX XML", style = "height: 35px; line-height: 0px")))
            ),
            
            fluidRow(
              column(12, 
                     tags$h3("Document summary"),
                     uiOutput(ns("vegx_summary")),
                     hr()
              )
            ),
            actionButton(ns("import"), label = "Import", width = "100px", class = "btn-success pull-right")
          )
        )
      ),
      tabPanel("Help",
        div(
          class = "content",
            tags$h1("Help with reading Veg-X"),
            div(class = "info-box",
               div(class = "text-info info-box-item",
                   icon("lightbulb", class = "icon-padded-right"),
                   tags$span(style = "font-size:1.8rem;", "Before a Veg-X 
                             document can be read in here, it must first 
                             be uploaded in the 'Start' section")),
           ),
           tags$p("Choose an uploaded Veg-X file for validation. Review the 
                 summary and run the import."),
          
        )
      )      
    )
  )  
}

#' vegxImport Server Functions
#'
#' @noRd 
mod_vegxImport_server <- function(id, user_data, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    upload_valid = reactiveVal(F)
    output$vegx_summary = renderText("No summary available.")
    
    dropdown_empty = reactive({
      if(length(names(user_data)) == 0){
        c("No files found" = "")
      } else {
        c("Choose a file" = "")
      }
    })
    
    # Observe and update input instead of using a reactive expression in the definition, thus preventing re-rendering of the entire UI when `user_data()` changes 
    observe({   
      file_selected = input$vegx_file # save current selection
      choices = names(user_data)[stringr::str_ends(names(user_data), ".xml")]
      updateSelectizeInput(session, inputId = "vegx_file", selected = file_selected, choices = c(dropdown_empty(), choices)) 
    })
    
    # Read Turboveg XML into tabular format
    observeEvent(
      eventExpr = input$read_vegx,
      handlerExpr = {  
        tryCatch({     
          shinyjs::disable("read_vegx")
          shinyjs::disable("vegx_file")

          if(!isTruthy(input$vegx_file)){
            showNotification("Please select a file.", type = "error")
            return()
          }
          vegx_schema_full = read_xml(system.file("extdata", "vegxschema", "veg.xsd", package = "VegXshiny"))
          if(length(xml_find_all(user_data[[input$vegx_file]], "../vegX")) == 0){
            showNotification("Uploaded file hase no root node named 'vegX'.", type = "error")
            stop()
          }
          output$vegx_summary = tryCatch({
            render_export_summary(user_data[[input$vegx_file]])
          }, error = function(e){
            renderText("No summary available")
          })
          
          upload_valid(T)
          showNotification("VegX document read.")
        }, error = function(e){
          upload_valid(F)
        }, finally = {
          shinyjs::enable("read_vegx")
          shinyjs::enable("vegx_file")
        })
      }
    )
    
    observeEvent(
      eventExpr = input$import, 
      handlerExpr = {
        if(upload_valid()){
          modal_content = div(class = "text-center text-info", icon("check"), tags$p("This will replace the current VegX document with the uploaded file."))
          modal_footer = tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")), 
                                   actionButton(ns("confirm_import"), class = "pull-right btn-success", "Proceed", icon("check")))
        } else {
          modal_content = div(class = "text-center text-danger", icon("exclamation"), tags$p("Please read in a valid VegX file."))
          modal_footer = tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")), 
                                   shinyjs::disabled(actionButton(ns("confirm_import"), class = "pull-right btn-success", "Proceed", icon("check"))))
        }
        
        # Show modal dialog
        showModal(
          modalDialog(size = "l",
                      modal_content,
                      footer = modal_footer)
        )
      }
    )
    
    observeEvent(
      eventExpr = input$dismiss_modal, 
      handlerExpr = {
        removeModal()
      }
    )
    
    observeEvent(
      eventExpr = input$confirm_import,
      handlerExpr = {
        tryCatch({
          # Get uploaded VegX file
          vegx_upload = user_data[[input$vegx_file]]
          
          # Remove attributes and child nodes from vegx_doc
          vegx_doc %>% xml_find_all("//vegX") %>% xml_children() %>% xml_remove()
          
          sapply(names(xml_attrs(vegx_doc)), function(attr){
            xml_set_attr(vegx_doc, attr, NULL)
          })
          
          # Append attributes and child nodes from vegx_upload to vegx_doc
          sapply(names(xml_attrs(vegx_upload)), function(attr){
            xml_set_attr(vegx_doc, attr, "1")
          })
          
          sapply(xml_children(vegx_upload), function(node){
            xml_add_child(vegx_doc, node)
          })
          
          # Update vegx_txt
          vegx_txt(as.character(vegx_doc))
          
          showNotification("Import finished.")
          new_action_log_record(log_path, "Import info", "Data imported from VegX file.")
          action_log(read_action_log(log_path))
        }, error = function(e){
          shiny::showNotification("Upload failed. Please consult the log for more information.", type = "error")
          new_action_log_record(log_path, "Import error", e$message)
          action_log(read_action_log(log_path))
        }, finally = {
          removeModal()
        })
      }
    )
  })
}
