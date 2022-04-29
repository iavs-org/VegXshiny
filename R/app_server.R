#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny 
#' @import golem 
#' @import xml2 
#' @import dplyr
#' @import bslib
#' @import stringr
#' 
#' @noRd 

app_server <- function(input, output, session) {
  log_path = tempfile("log_", fileext = ".csv")
  new_action_log_record(log_path, "System info", "Session started", append = F, col.names = T)
  
  # ---------------------------------------------------------------------------------------- #
  # Create xml objects
  # VegX schema
  schema_files = load_schema()
  vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
  link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)
  
  # VegX document
  vegx_doc = new_vegx_document()
  # vegx_doc_history = list(vegx_doc)

  # ---------------------------------------------------------------------------------------- #
  # Create global reactive values
  vegx_txt = reactiveVal({
    as.character(vegx_doc)
  })
  
  action_log = reactiveVal({
    read_action_log(log_path)
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
          node_hovered = xml_find_all(vegx_schema, xpath)
          
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
      shiny::showNotification("Error in tree selection. Please contact the authors.", type = "error")
    })
  })
  
  # --------------------------------------------------------------------------------------- #
  # About page
  mod_about_server("about")
  
  # --------------------------------------------------------------------------------------- #
  # File Upload
  user_data = mod_fileManager_server("fileManager", action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Guided import for tabular data
  mod_importWizard_server("importWizard", user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Create mappings and add nodes freely
  mod_vegxBuilder_server("vegxBuilder", user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # XML Viewer
  mod_xmlViewer_server("xmlViewer", vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Action Log
  mod_actionLog_server("actionLog", action_log)
}
