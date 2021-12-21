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
  
  mod_elementControl_server("elementControl_ui_1")  
  #mod_elementSelect_server("elementSelect_ui_1")
}
