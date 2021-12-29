#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import golem
#' @noRd

app_server <- function( input, output, session ) {
  # Server logic
  library(shinyTree) # package doesnt work otherwise...
  
  mod_about_server("about_ui_1")
  user_data = mod_fileManagement_server("fileManagement_ui_1")
  mod_elementControl_server("elementControl_ui_1", user_data = user_data)  
}
