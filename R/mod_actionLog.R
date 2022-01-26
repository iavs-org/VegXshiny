#' actionLog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_actionLog_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' actionLog Server Functions
#'
#' @noRd 
mod_actionLog_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_actionLog_ui("actionLog_ui_1")
    
## To be copied in the server
# mod_actionLog_server("actionLog_ui_1")
