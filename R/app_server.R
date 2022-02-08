#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny 
#' @import golem 
#' @import xml2 
#' @import dplyr
#' @import bslib
#' @import shinyjs 
#' @import stringr
#' 
#' @noRd 

app_server <- function( input, output, session ) {
  library(shinyTree) # package doesn't work otherwise
  library(shinyBS)   # same

  # Cleanup function
  log_path = paste0("inst/app/www/logs/log_", session$token, ".csv")
  onStop(function(){
    unlink(log_path)
  })
  
  # ----------------------<----------------------------------------------------------------- #
  # Create global reactive values
  vegx_txt = reactiveVal({
    tmp = tempfile(fileext = ".xml")
    write_xml(vegx_doc, tmp, options = "format")
    readChar(tmp, file.info(tmp)$size)
  })
  
  action_log = reactiveVal({
    log = data.frame(
      timestamp = as.character(Sys.time()),
      type      = "Info",
      message   = "Session started."
    )
    write.table(log, file = paste0("inst/app/www/logs/log_", session$token, ".csv"), row.names = F)
    log
  })
  
  # --------------------------------------------------------------------------------------- #
  # Create global observers
  observe({
    tryCatch({
      if(length(input$nodeHoveredInfo) != 0){
        node_lineage = unlist(input$nodeHoveredInfo$nodeLineage)
        if(node_lineage[1] != "choice"){
          xpath = paste(sapply(rev(node_lineage), function(parent){
            paste0("//*[@name='", parent, "']")
          }), collapse = "")
          node_hovered = xml_find_all(vegx_schema_simple, xpath)
          
          node_attributes = as.list(xml_attrs(node_hovered[[1]]))
          if(is.null(node_attributes[["minOccurs"]])){node_attributes$minOccurs = 1}
          if(is.null(node_attributes[["maxOccurs"]])){node_attributes$maxOccurs = 1}
          node_attributes = node_attributes[order(names(node_attributes))]
          node_info = list(
            tree = input$nodeHoveredInfo$tree,
            nodeId = input$nodeHoveredInfo$nodeId,
            nodeAttributes = node_attributes
          )
          session$sendCustomMessage("node_tooltip", node_info)  
        }
      }
    }, error = function(e){
      print(e) # TODO add to action log
    })
  })
  
  # --------------------------------------------------------------------------------------- #
  # About page
  mod_about_server("about")
  
  # --------------------------------------------------------------------------------------- #
  # File Upload
  user_data = mod_fileManagement_server("fileManagement", action_log)
  
  # --------------------------------------------------------------------------------------- #
  # Main UI: Create mappings and build VegX document
  mod_documentCreation_server("documentCreation", user_data, vegx_txt, action_log)
  
  # --------------------------------------------------------------------------------------- #
  # XML Viewer
  mod_viewXML_server("viewXML", vegx_txt, action_log)
  
  # --------------------------------------------------------------------------------------- #
  # Action Log
  mod_actionLog_server("actionLog", action_log)
}
