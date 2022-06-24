#' aboutVegXshiny UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_aboutVegXshiny_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$p("Nothing to see here yet.")
  )
}
    
#' aboutVegXshiny Server Functions
#'
#' @noRd 
mod_aboutVegXshiny_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}