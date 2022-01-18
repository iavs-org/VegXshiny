#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny golem 
#' @noRd 

app_server <- function( input, output, session ) {
  # Server logic
  library(shinyTree) # package doesn't work otherwise
  library(shinyBS)   # same
  
  # About page
  mod_about_server("about")
  
  # File Upload
  user_data = mod_fileManagement_server("fileManagement")
  
  # Main UI: Create mappings and build VegX document
  annotation_text = reactiveVal() # This reactive is populated by a custom JS observer on the shinyTree nodes; doesn't really work with modules, thus here
  observeEvent(eventExpr = input$node_hovered_parents, 
               handlerExpr = {
                 xpath = paste(sapply(rev(input$node_hovered_parents), function(parent){
                   paste0("//*[@name='", parent, "']")
                 }), collapse = "")
                 annotation = xml_attr(xml_find_all(vegx_schema_simple, xpath), "annotation")
                 annotation = ifelse(is.na(annotation), "No information available.", annotation)
                 annotation_text(annotation)
               })
  
  mod_documentCreation_server("documentCreation", user_data, annotation_text)
  
  # XML Viewer
  vegx_txt = reactiveVal({
    tmp = tempfile(fileext = ".xml")
    write_xml(vegx_doc, tmp, options = "format")
    readChar(tmp, file.info(tmp)$size)
  })
  
  mod_viewXML_server("viewXML", vegx_txt)
}