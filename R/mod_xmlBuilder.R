#' xmlBuilder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_xmlBuilder_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    actionButton(ns("submit"), label = "Submit", class = "btn-primary"),
  )
}

#' xmlBuilder Server Functions
#'
#' @noRd 
mod_xmlBuilder_server <- function(id, tabs_shown, tab_selected, elem_selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ns_navbar = session$userData$ns_navbar
    
    # Submit button
    observeEvent(eventExpr = input$submit,
                 handlerExpr = {
                   # TODO 
                   # --> produce xml and add to VegX document
                   # --> update action log and progress tab 
                   
                   # Update Style
                   shinyBS::updateButton(session = session, inputId = ns_navbar(tab_selected$current), icon = icon("check", class = "icon-padded"), style =  "success")
                   
                   # Select new tab
                   isolate({tabs_shown = sort(tabs_shown())})
                   new_tab_index = (which(tabs_shown == tab_selected$current)) %% length(tabs_shown) + 1
                   shinyjs::click(id = ns_navbar(tabs_shown[new_tab_index]), asis = T)
                 })
  })
}