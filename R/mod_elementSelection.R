#' elementSelection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shinyTree

mod_elementSelection_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
  )
}

#' elementSelection Server Functions
#'
#' @noRd 
mod_elementSelection_server <- function(id, tab_selected, elem_selected){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$tree, {
      selected = shinyTree::get_selected(input$tree, "slices") # Get selected elements
      selected = names(unlist(selected))
      is_leaf = stringr::str_extract(selected, "[^.]+$") %in% vegx_leaf_elements # Check which selected elements are leaf nodes
      elem_selected[[tab_selected]] = selected[is_leaf] # Update elem_selected with selected leaf nodes
    })
    
    # Render UI for selected VegX element
    output$tree = shinyTree::renderTree({
      vegx_to_list(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_selected, "']"))), "name")
    })
  })
}