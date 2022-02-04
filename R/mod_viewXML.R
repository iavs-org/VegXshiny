#' viewXML UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shinyAce aceEditor updateAceEditor
mod_viewXML_ui <- function(id){
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 tagList(
                   downloadButton(ns("export"), "Export",  class = "btn-sidebar", style = "width:100%"),
                   actionButton(ns("edit"), "Edit", class = "btn-sidebar", width = "100%"),
                   actionButton(ns("validate"), "Validate", class = "btn-sidebar", width = "100%")
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

#' viewXML Server Functions
#'
#' @noRd 
mod_viewXML_server <- function(id, vegx_txt, action_log){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      updateAceEditor(session, "xml_viewer", value = vegx_txt())
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
