#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import golem
#' @noRd

app_server <- function( input, output, session ) {
  # Server logic
  library(shinyTree) # package doesn't work otherwise
  library(shinyBS)   # same
  
  # Build about page
  mod_about_server("about")
  
  # Get uploaded files
  user_data = mod_fileManagement_server("fileManagement")
  
  # Main UI: Create mappings and build VegX document
  info_text = reactiveVal() # This reactive is populated by custom JS observer on the shinyTree nodes
  observeEvent(eventExpr = input$node_hovered_parents, 
               handlerExpr = {
                 xpath = paste(sapply(rev(input$node_hovered_parents), function(parent){
                   paste0("//*[@name='", parent, "']")
                 }), collapse = "")
                 annotation = xml_attr(xml_find_all(vegx_smpl, xpath), "annotation")
                 annotation = ifelse(is.na(annotation), "No information available.", annotation)
                 info_text(annotation)
               })
  
  mod_documentCreation_server("documentCreation", user_data, info_text)
  
  
}
