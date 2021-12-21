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
               theme = shinytheme("yeti"),
               
               tabPanel("About"),
               
               tabPanel("Manage Files",
                        fileInput("file", "Choose CSV files", accept = ".csv", multiple = T),
               ),
               
               tabPanel("Create VegX",
                        mod_elementControl_ui("elementControl_ui_1")
               ),
               tabPanel("Check Status", ),
               tabPanel("Export VegX")
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

