#' elementSelect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_elementSelect_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyTree("vegx_tree", theme = "proton", checkbox = T,  multiple = T, themeIcons = F)
  )
}
    
#' elementSelect Server Functions
#'
#' @noRd 
mod_elementSelect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_elementSelect_ui("elementSelect_ui_1")
    
## To be copied in the server
# mod_elementSelect_server("elementSelect_ui_1")
