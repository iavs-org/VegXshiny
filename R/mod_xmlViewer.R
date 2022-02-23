#' xmlViewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shinyAce aceEditor updateAceEditor
mod_xmlViewer_ui <- function(id){
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 tagList(
                   actionButton(ns("edit"), "Edit", class = "btn-sidebar", width = "100%"),
                   actionButton(ns("validate"), "Validate", class = "btn-sidebar", width = "100%"),
                   downloadButton(ns("export"), "Export",  class = "btn-sidebar", style = "width:100%")
                 )
    ),
    mainPanel(width = 10, 
              tagList(
                aceEditor(
                  outputId = ns("xml_viewer"), 
                  value = "",
                  mode = "xml",
                  theme = "tomorrow",
                  readOnly = T,
                  autoComplete = "disabled"
                ),
              )
    )
  )
}

#' xmlViewer Server Functions
#'
#' @noRd 
mod_xmlViewer_server <- function(id, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      updateAceEditor(session, "xml_viewer", value = vegx_txt())
    })
    
    observeEvent(eventExpr = input$edit,
                 handlerExpr = {
                   showModal(
                     modalDialog(easyClose = T,
                                 tags$label("Warning", style = "color:red"),
                                 tags$p("Editing raw XML may corrupt the document and is recommended for expert use only."),
                                 footer = tagList(
                                   tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                             actionButton(ns("confirm_edit"), class = "pull-right btn-success", "Understood", icon("check")))
                                 ),
                                 size = "l",
                     )
                   )
                 })
    
    observeEvent(eventExpr = input$dismiss_modal, 
                 handlerExpr = {
                   removeModal()
                 })
    
    
    observeEvent(eventExpr = input$confirm_edit,
                 handlerExpr = {
                   insertUI(selector = paste0("#", ns("edit")), 
                            where = "afterEnd",
                            ui = div(
                              id = ns("edit_controls"),
                              tags$span(
                                actionButton(ns("discard_edits"), "Discard", class = "btn-sidebar pull-left btn-danger", icon = icon("times"), width = "49%"),
                                actionButton(ns("save_edits"), "Save edits", class = "btn-sidebar pull-right btn-success",  icon("check"), width = "49%"))
                            )
                   )
                   removeUI(selector = paste0("#", ns("edit"))) 
                   updateAceEditor(session, "xml_viewer", readOnly = F)
                   removeModal()
                 })
    
    observeEvent(eventExpr = input$save_edits,
                 handlerExpr = {
                   tryCatch({
                     vegx_doc_edits = read_xml(isolate(input$xml_viewer)) %>% xml_find_all("//vegX") %>% xml_children()
                     vegx_doc %>% xml_find_all("//vegX") %>% xml_children() %>% xml_remove()
                     sapply(vegx_doc_edits, function(node){
                       xml_add_child(vegx_doc, node)
                     })
                     vegx_txt(as.character(vegx_doc))
                     
                     insertUI(selector = paste0("#", ns("edit_controls")), 
                              where = "afterEnd",
                              ui = actionButton(ns("edit"), "Edit", class = "btn-sidebar", width = "100%"))
                     removeUI(selector = paste0("#", ns("edit_controls"))) 
                     updateAceEditor(session, "xml_viewer", readOnly = T)
                   }, error = function(e){
                     shiny::showNotification("Document error. Please consult the log for more information.")
                     new_action_log_record(log_path, "Document error", paste0("Document edit failed with the following exception:<ul><li>", e, "</li></ul>"))
                     action_log(read_action_log(log_path))
                   })
                 })
    
    observeEvent(eventExpr = input$discard_edits,
                 handlerExpr = {
                   insertUI(selector = paste0("#", ns("edit_controls")), 
                            where = "afterEnd",
                            ui = actionButton(ns("edit"), "Edit", class = "btn-sidebar", width = "100%"))
                   removeUI(selector = paste0("#", ns("edit_controls"))) 
                   updateAceEditor(session, "xml_viewer", value = vegx_txt(), readOnly = T)
                 })
    
    observeEvent(eventExpr = input$validate,
                 handlerExpr = {
                   vegx_schema_full = read_xml(system.file("extdata", "vegxschema", "veg.xsd", package = "VegXshiny"))
                   is_valid = xml2::xml_validate(vegx_doc, schema = vegx_schema_full)
                   if(is_valid){
                     shiny::showNotification("Validation successful.", type = "message")
                     new_action_log_record(log_path, "Validation info", "Document validation successful")
                   } else {
                     shiny::showNotification("Validation error. Please consult the log for more information.", type = "error")
                     new_action_log_record(log_path, "Validation error", paste0("Document validation failed with the following exceptions: <ul>", 
                                                                                paste0("<li>Error: ", attr(is_valid, "errors"), "</li>", collapse = ""), "</ul>"))
                   }
                   action_log(read_action_log(log_path))
                 })
    
    output$export = downloadHandler(
      filename = "vegx.xml",
      content = function(file) {
        write_xml(vegx_doc, file)
      },
      contentType = "xml"
    )
  })
}
