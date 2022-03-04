#' quickformGenerator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quickformGenerator_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_form"))
  )
}

#' quickformGenerator Server Functions
#'
#' @noRd 
mod_quickformGenerator_server <- function(id, elem_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$input_form = switch(
      elem_name,
      "party" = {
        renderUI(
          tagList(
            fluidRow(
              column(width = 4,
                     textInput(ns("party_name"), "Personnel name")     
              ),
              column(width = 6,
                     radioButtons(ns("party_role"), label = "Role", choices = list("Individual", "Organization", "Position"), inline = T)
              )
            )
          )
        )
      }
    )                          
  })
}

## To be copied in the UI
# mod_quickformGenerator_ui("quickformGenerator_ui_1")

## To be copied in the server
# mod_quickformGenerator_server("quickformGenerator_ui_1")
