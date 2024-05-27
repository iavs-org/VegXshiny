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
    tabsetPanel(
      tabPanel("Review Veg-X",

        tags$h1 ("Inspect, edit and validate your current Veg-X document"),
        column(
          width = 12,
          fluidRow(
            actionButton(ns("edit"), "Edit", width = "80px", class = "btn-xs"),
            actionButton(ns("validate"), "Validate", width = "80px", class = "btn-xs")
          ),
          fluidRow(
            aceEditor(
              outputId = ns("xml_viewer"), 
              value = "",
              mode = "xml",
              theme = "tomorrow",
              readOnly = T,
              autoComplete = "disabled",
              autoScrollEditorIntoView = TRUE
            )
          )
        )
      ),
      
      tabPanel("Help",
        div(
          class = "content",
          tags$h1("Help with reviewing Veg-X"),
           
          tags$p("Click on the 'Edit' button if you need to modify the raw XML 
                 of the current Veg-X document. Caution is advised, as 
                 manually editing a Veg-X file can quickly invalidate it. Note 
                 that edits cannot be undone once saved."),
          tags$p("ressing the 'Validate' button tests whether the current Veg-X 
                 document conforms to the Veg-X XML Schema and whether there are 
                 any potential problems with references to nodes. Validation 
                 errors and other problems are listed in the action log.")      
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
                   
                   if(length(references_valid) == 0){
                     msg_type = "info"
                     msg_references = "No issues related to internal ID references found."  
                   } else {
                     msg_type = "warning"
                     msg_references = paste0("Potential issues found related to internal ID references: ", 
                                             "<ul>", paste0("<li>", references_valid, "</li>", collapse = ""), "</ul>")
                   }
                   
                   if(schema_valid){
                     msg_type = ifelse(msg_type == "warning", msg_type, "info")
                     msg_val = "<p>Veg-X document successfully validated against schema.</p>"
                   } else {
                     msg_type = "error"
                     msg_val = paste0("Veg-X document validation failed with the following exceptions:", 
                                      "<ul>", paste0("<li>Error: ", attr(schema_valid, "errors"), "</li>", collapse = ""), "</ul>")
                   }
                   
                   # Update log
                   msg = paste0(msg_val, msg_references)
                   new_action_log_record(log_path, paste0("Validation ", msg_type), msg)
                   action_log(read_action_log(log_path))
                   
                   switch(msg_type,
                          "info" = shiny::showNotification("Validation successful.", type = "message"),
                          "warning" = shiny::showNotification("Validation successful but potential issues with ID references found. Please consult the log for more information.", type = "warning"),
                          "error" = shiny::showNotification("Validation failed. Please consult the log for more information.", type = "error")
                   )
                 })
  })
}
