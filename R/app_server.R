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
  upload_size = get_golem_options("max_upload_size_MB")
  options(shiny.maxRequestSize = upload_size * 1024^2) # Set max upload size
  
  log_path = tempfile("log_", fileext = ".csv")
  new_action_log_record(log_path, "System info", "Session started.", append = F, col.names = T)
  
  # ---------------------------------------------------------------------------------------- #
  # Create xml objects
  # VegX schema
  schema_files = load_schema()
  vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
  link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)
  
  # VegX document
  vegx_doc = new_vegx_document()
  
  # ---------------------------------------------------------------------------------------- #
  # Create global reactive values
  vegx_txt = reactiveVal({
    as.character(vegx_doc)
  })
  
  action_log = reactiveVal({
    read_action_log(log_path)
  })
  
  templates = reactiveVal({
    templates_predefined
  })
  
  templates_lookup = reactiveVal({
    templates_predefined_lookup
  })
  
  file_order = reactiveVal()       
  # --------------------------------------------------------------------------------------- #
  # Create global observers
  node_info = reactiveVal()
  observe({
    try({
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
          node_info_hovered = list(
            node_lineage = rev(node_lineage),
            node_attributes = node_attributes
          )
          node_info(node_info_hovered)
        }
      }
    })
  })
  
  observe({
    browser_info = shinybrowser::get_all_info()
    if(browser_info$dimensions$width < 800 | browser_info$device == "Mobile"){
      showModal(
        modalDialog(
          div(class = "text-center text-danger", tags$h4("Your display dimensions may not be fully compatible with this application.")),
          tags$p("Consider resizing you browser window or running VegXShiny on a device with a larger screen."),
          size = "m", easyClose = T)
      )
    }
  })
  
  # --------------------------------------------------------------------------------------- #
  # About
  mod_aboutVegX_server("about", vegx_schema, node_info)
  
  # --------------------------------------------------------------------------------------- #
  # File Upload
  user_data = mod_fileManager_server("fileManager", file_order, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Import Wizard
  
  # 1. Tabular Data
  mod_tableImport_server("tableImport", file_order, user_data, vegx_schema, vegx_doc, vegx_txt, templates, templates_lookup, action_log, log_path)
  
  # 2. Turboveg XML Data
  mod_turbovegImport_server("turbovegImport", user_data, vegx_schema, vegx_doc, vegx_txt, templates, templates_lookup, action_log, log_path)
  
  # 3. VegX XML Data
  mod_vegxImport_server("vegxImport", user_data, vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # XML Viewer
  mod_xmlViewer_server("xmlViewer", vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Export
  mod_vegxExport_server("vegxExport", vegx_doc, vegx_txt, action_log, log_path)
  
  # --------------------------------------------------------------------------------------- #
  # Action Log
  mod_actionLog_server("actionLog", action_log)
}
