#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#'  
#' @importFrom shinythemes shinytheme
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    navbarPage("VegXshiny",  
               theme = shinytheme("darkly"),
               
               tabPanel("About",
                        mod_about_ui("about")
               ),
               
               tabPanel(div(icon("folder-open", class = "icon-padded"), "File Manager"),
                        mod_fileManagement_ui("fileManagement")
               ),
               
               # TODO TEMPLATE BUILDER?!

               tabPanel(div(icon("leaf", class = "icon-padded"), "VegX Builder"),
                        mod_documentCreation_ui("documentCreation")
               ),
               
               tabPanel(div(icon("file", class = "icon-padded"), "XML Viewer"),
                        mod_viewXML_ui("viewXML")
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

