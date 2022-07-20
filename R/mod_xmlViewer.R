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
  
  fluidPage(
    column(
      width = 10, offset = 1,
      fluidRow(
        tags$p("Inspect, edit and validate your current VegX document.", class = "text-info annotation")
      ),
      fluidRow(
        actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"),
        actionButton(ns("validate"), "Validate", width = "80px", class = "btn-xs")
      ),
      fluidRow(
        aceEditor(
          outputId = ns("xml_viewer"), 
          value = "",
          height = "80vh",
          mode = "xml",
          theme = "tomorrow",
          readOnly = T,
          autoComplete = "disabled"
        )
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
                                   tags$span(actionButton(ns("dismiss_modal"), "Abort", class = "pull-left btn-danger", icon = icon("times")), 
                                             actionButton(ns("confirm_edit"), class = "pull-right btn-success", "Proceed", icon("check")))
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
                            ui = tagList(
                              actionButton(ns("save_edits"), "Save edits", class = "btn-success", class = "btn-xs", width = "80px", icon("check")),
                              actionButton(ns("discard_edits"), "Discard", class = "btn-danger", class = "btn-xs", width = "80px", icon = icon("times"))
                            )
                   )
                   removeUI(selector = paste0("#", ns("edit"))) 
                   updateAceEditor(session, "xml_viewer", readOnly = F)
                   removeModal()
                 })
    
    observeEvent(eventExpr = input$save_edits,
                 handlerExpr = {
                   tryCatch({
                     # Read edits
                     vegx_doc_edits = read_xml(isolate(input$xml_viewer)) %>% xml_find_all("//vegX") %>% xml_children()
                     
                     # Remove all nodes except root from vegx document
                     vegx_doc %>% xml_find_all("//vegX") %>% xml_children() %>% xml_remove()
                     
                     # Append all nodes from edited document to vegx document
                     sapply(vegx_doc_edits, function(node){
                       xml_add_child(vegx_doc, node)
                     })
                     vegx_txt(as.character(vegx_doc))
                     
                     # Restore UI state
                     insertUI(selector = paste0("#", ns("save_edits")),
                              where = "beforeBegin",
                              ui = actionButton(ns("edit"), "Edit", class = "btn-xs", width = "80px"))
                     removeUI(selector = paste0("#", ns("save_edits")))
                     removeUI(selector = paste0("#", ns("discard_edits")))
                     updateAceEditor(session, "xml_viewer", readOnly = T)
                     
                     shiny::showNotification("Edits saved.")
                     new_action_log_record(log_path, "Document info", paste0("Saved manual edits to XML document."))
                   }, error = function(e){
                     shiny::showNotification("Document error. Please consult the log for more information.")
                     new_action_log_record(log_path, "Document error", paste0("Document edit failed with the following exception:<ul><li>", e, "</li></ul>"))
                   }, finally = {
                     # Update action log
                     action_log(read_action_log(log_path))
                   })
                 })
    
    observeEvent(eventExpr = input$discard_edits,
                 handlerExpr = {
                   # Restore UI state
                   removeUI(selector = paste0("#", ns("save_edits")))
                   removeUI(selector = paste0("#", ns("discard_edits")))
                   insertUI(selector = paste0("#", ns("validate")),
                            where = "beforeBegin",
                            ui = actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"))
                   updateAceEditor(session, "xml_viewer", value = vegx_txt(), readOnly = T)
                 })
    
    observeEvent(eventExpr = input$validate,
                 handlerExpr = {
                   vegx_schema_full = read_xml(system.file("extdata", "vegxschema", "veg.xsd", package = "VegXshiny"))
                   schema_valid = xml2::xml_validate(vegx_doc, schema = vegx_schema_full)
                   references_valid = check_document_links(vegx_doc)

                   msg_references = "No issues related to internal ID references found."
                   if(!length(references_valid) == 0){
                     msg_references = paste0("Potential issues found related to internal ID references: ",
                                            "<ul>", paste0("<li>", references_valid, "</li>", collapse = ""), "</ul>")
                   }
                   
                   if(schema_valid){
                     shiny::showNotification("Validation successful.", type = "message")
                     msg_type = "Validation info"
                     msg = paste0("<p>Document successfully validated against VegX schema.</p>", msg_references)
                   } else {
                     shiny::showNotification("Validation failed. Please consult the log for more information.", type = "error")
                     msg_type = "Validation error"
                     msg_val = paste0("Document validation against VegX schema failed with the following exceptions:", 
                                      "<ul>", paste0("<li>Error: ", attr(schema_valid, "errors"), "</li>", collapse = ""), "</ul>")
                     msg  = paste0(msg_val, msg_references)
                   }
                   
                   # Update action log
                   new_action_log_record(log_path, msg_type, msg)
                   action_log(read_action_log(log_path))
                 })
  })
}
