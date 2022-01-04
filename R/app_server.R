#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import golem
#' @noRd

app_server <- function( input, output, session ) {
  # Server logic
  library(shinyTree) # package doesn't work otherwise
  library(shinyBS)   # same
  
  # Build about page
  mod_about_server("about")
  
  # Get uploaded files
  user_data = mod_fileManagement_server("fileManagement")
  
  # Create mappings and build VegX document
  mod_documentCreation_server("documentCreation", user_data)
}
