#' rowGenerator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rowGenerator_ui <- function(id){
  ns <- NS(id)
  div(id = ns("mapping"),
      fluidRow(
        column(5, uiOutput(ns("element_ui"))),
        column(5, uiOutput(ns("value_ui"))),
        column(2,
               align = "center",
               column(9, shinyWidgets::radioGroupButtons(ns("source"), label = NULL, choices = c("Upload", "Freetext"), status = "info", size = "s", justified = T)),
               column(3, actionButton(ns("remove"), label = NULL, icon = icon("times"), width = "34px", style = "height:34px; padding:0"))
        )
      )
  )
}

#' rowGenerator Server Functions
#'
#' @noRd 
mod_rowGenerator_server <- function(id, tab_selected, elem_selected, data_columns, fields_used, vegx_mappings, mapping_count){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # SelectizeInput VegX elements
    output$element_ui = renderUI({
      elem_used = unlist(fields_used[["elements"]])           # Get elements used in Mapping UI
      if(id %in% names(elem_used)){
        selected = elem_used[id]                           
        elem_used_mod = elem_used[names(elem_used) != id]                     # Ignore current id (otherwise any choice in the selectize would be removed again immediately)
        elem_choices = setdiff(elem_selected[[tab_selected]], elem_used_mod)  # remove all but the currently used element from choices
      } else {
        selected = NULL
        elem_choices = setdiff(elem_selected[[tab_selected]], elem_used)
      }
      selectInput(ns("element"), label = NULL, width = "100%", selected = selected, choices = c("Select elements from above" = "", elem_choices))
    })
    
    # SelectizeInput or TextInput values to be mapped to VegX element
    output$value_ui   = renderUI({
      if(input$source == "Upload"){
        selectInput(ns("value"), label = NULL, width = "100%", choices = c("Select columns from uploaded data" = "", data_columns()))
      } else {
        textAreaInput(ns("value"), label = NULL, placeholder = "Enter free text", height = "34px", width = "100%", resize = "vertical")
      }
    })
    
    observeEvent(eventExpr = {input$element; input$value}, 
                 handlerExpr = {
                   fields_used[["elements"]][[id]] = input$element
                   fields_used[["values"]][[id]] = input$value
                   
                   # Remove used element and value from other 
                   if(input$element != "" & input$value != ""){
                     vegx_mappings[[tab_selected]][[input$element]] = input$value
                   }
                   
                   # print("----- fields used ------")
                   # print(reactiveValuesToList(fields_used))
                   # print("----- vegx mappings ------")
                   # print(reactiveValuesToList(vegx_mappings))
                 })
    
    observeEvent(eventExpr = input$remove,
                 handlerExpr = {
                   if(mapping_count() > 1){
                     removeUI(paste0("#", ns("mapping")))
                     mapping_count(mapping_count() - 1)
                     fields_used[["elements"]][[id]] = NULL
                     fields_used[["values"]][[id]] = NULL
                     vegx_mappings[[tab_selected]][[input$element]] = NULL
                     print("----- fields used ------")
                     print(reactiveValuesToList(fields_used))
                     print("----- vegx mappings ------")
                     print(reactiveValuesToList(vegx_mappings))
                   } else {
                     shiny::showNotification("Can't remove last row.", type = "warning")
                   }
                 })
  })
}