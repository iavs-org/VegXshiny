#' viewXML UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom xml2 xml_new_root xml_set_attrs write_xml
mod_viewXML_ui <- function(id){
  ns <- NS(id)
  tagList(
    aceEditor(
      outputId = ns("xml_viewer"), 
      value = "",
      mode = "xml",
      theme = "tomorrow",
      readOnly = T,
      autoComplete = "disabled"
    ),
    downloadButton(ns("export"), "Export"),
    actionButton(ns("edit"), "Edit")
  )
}

#' viewXML Server Functions
#'
#' @noRd 
mod_viewXML_server <- function(id, vegx_text){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      print(vegx_text())
      updateAceEditor(session, "xml_viewer", value = vegx_text())
    })
    
    output$export = downloadHandler(
      filename = "vegx.xml",
      content = function(file) {
        write_xml(vegx_doc, file)
      },
      contentType = "xml"
    )
    
    observeEvent(eventExpr = input$edit,
                 handlerExpr = {
                   updateAceEditor(session, "xml_viewer", readOnly = F)
                 })
  })
}