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
    
    # Include the custom.js file
    tags$head(tags$script(src = "www/custom.js")),
    
    # Detect device & browser info
    shinybrowser::detect(),
    
    # Application UI logic 
    navbarPage("",
               position = "static-top",
               theme=bslib::bs_theme(version = 3, bootswatch = "flatly"),
               header = tags$style(type = "text/css", 
                                   ".navbar-header { display: none; }",
                                   ".dropdown-menu { font-size: 0.9em; }"
               ),   
               tabPanel("VegXshiny", mod_aboutVegX_ui("about")),
               
               tabPanel("Start", icon = icon("file-import", class = "icon-padded-right"),
                        mod_fileManager_ui("fileManager")
               ),
               
               navbarMenu(title = "Import", icon = icon("wand-sparkles", class = "icon-padded-right"), 
                          tabPanel("Table", mod_tableImport_ui("tableImport")),
                          tabPanel("Turboveg", mod_turbovegImport_ui("turbovegImport")),
                          tabPanel("Veg-X", mod_vegxImport_ui("vegxImport"))
               ),
               
               tabPanel("Review", icon = icon("eye", class = "icon-padded-right"),
                        mod_xmlViewer_ui("xmlViewer")
               ),
               
               tabPanel("Download", icon = icon("file-export", class = "icon-padded-right"),
                        mod_vegxExport_ui("vegxExport")
               ),
               
               tabPanel("Action Log", icon = icon("stethoscope", class = "icon-padded-right"),
                        mod_actionLog_ui("actionLog")
               )
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

