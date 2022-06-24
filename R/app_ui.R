#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#'  
#' @importFrom bslib bs_theme
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Application UI logic 
    navbarPage("VegXshiny",  
               position = "fixed-top",
               theme=bslib::bs_theme(version = 3, bootswatch = "darkly"),
               header = tags$style(type="text/css", "body {padding-top: 80px;}"),
               
               
               navbarMenu(title = "About ", icon = icon("home", class = "icon-padded-right"),
                          tabPanel("The VegX standard", mod_aboutVegX_ui("aboutVegX")),
                          tabPanel("How to use this app", mod_aboutVegXshiny_ui("aboutVegXshiny"))
               ),
               
               
               tabPanel("File Manager", icon = icon("folder-open", class = "icon-padded-right"),
                        mod_fileManager_ui("fileManager")
               ),
               
               navbarMenu(title = "Import wizard ", icon = icon("magic", class = "icon-padded-right"), 
                 tabPanel("Import from Tables", mod_tableImport_ui("tableImport")),
                 tabPanel("Import from TurboVeg XML", mod_turbovegImport_ui("turbovegImport"))
               ),
               
               tabPanel("Node editor",  icon = icon("project-diagram", class = "icon-padded-right"),
                        mod_vegxBuilder_ui("vegxBuilder")
               ),
                
               tabPanel("XML Viewer", icon = icon("code", class = "icon-padded-right"),
                        mod_xmlViewer_ui("xmlViewer")
               ),
               
               tabPanel("VegX Export", icon = icon("file-export", class = "icon-padded-right"),
                        mod_vegxExport_ui("vegxExport")
               ),
               
               tabPanel("Action Log", icon = icon("history", class = "icon-padded-right"),
                        mod_actionLog_ui("actionLog")
               ),
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'VegXshiny'
    )
  )
}

