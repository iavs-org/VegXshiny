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
    
    # Your application UI logic 
    navbarPage("VegXshiny",  
               position = "fixed-top",
               theme = bslib::bs_theme(bootswatch = "journal"),
               header = tags$style(type="text/css", "body {padding-top: 70px;}"),
               
               tabPanel("About",
                        mod_about_ui("about")
               ),
               
               tabPanel(div(icon("folder-open", class = "icon-padded"), "File Manager"),
                        mod_fileManager_ui("fileManager")
               ),
               
               navbarMenu("VegX Builder", icon = icon("leaf", class = "icon-padded"),
                          tabPanel("Import helper", h1("import some shit")),
                          tabPanel("Element editor", mod_vegxBuilder_ui("vegxBuilder")),
                          tabPanel("Template editor", h1("Build a template"))
               ),
               
               tabPanel(div(icon("file", class = "icon-padded"), "XML Viewer"),
                        mod_xmlViewer_ui("xmlViewer")
               ),
               
               tabPanel(div(icon("history", class = "icon-padded"), "Action Log"),
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

