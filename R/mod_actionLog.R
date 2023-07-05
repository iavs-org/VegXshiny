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
  
  fluidPage(
    fluidRow(
      column(
        width = 10, offset = 1,
        DT::dataTableOutput(ns("action_log"))
      )
    )
  )
}

#' actionLog Server Functions
#'
#' @noRd 
mod_actionLog_server <- function(id, action_log){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$action_log <- DT::renderDataTable(expr = action_log(), 
                                             selection = 'none',
                                             rownames = F,
                                             escape = F,
                                             options = list(paging=FALSE, columnDefs = list(list(width = '180px', targets = c(0,1)))))
  })
}