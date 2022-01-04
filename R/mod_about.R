#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import shiny
#' @importFrom knitr knit
#' @importFrom markdown markdownToHTML
mod_about_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2),
      column(8,
             tagList(
               uiOutput(ns("about_ui"))
             )
      ),
      column(2)
    )
  )
}

#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$about_ui = renderUI({
      shiny::HTML(markdown::markdownToHTML(knitr::knit('R/about.Rmd', quiet = TRUE), fragment.only = T))
    })
  })
}