#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import shinythemes
#' @importFrom shinyTree shinyTree
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    navbarPage("VegXShiny",  
               theme = shinytheme("darkly"),
               
               tabPanel("About",
                        mod_about_ui("about_ui_1")
               ),
               
               tabPanel(div(icon("file", class = "icon-padded"), "Manage Files"),
                        mod_fileManagement_ui("fileManagement_ui_1")
               ),
               
               tabPanel(div(icon("leaf", class = "icon-padded"), "Create VegX"),
                        mod_elementControl_ui("elementControl_ui_1")
               ),
               tabPanel(div(icon("chart-bar", class = "icon-padded"), "Check Progress")),
               tabPanel(div(icon("download", class = "icon-padded"), "Export VegX"))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

