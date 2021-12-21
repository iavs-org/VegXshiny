#' elementControl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import shinyTree
mod_elementControl_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   tagList(
                     lapply(vegx_names, function(name){
                       actionButton(inputId = ns(name), label = name, width = "100%", class = "btn btn-sm")
                     })  
                   )
      ), 
      mainPanel(
        tagList(
          h2("Select VegX elements"),
          shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, search = T),
          hr(),
          h2("Create mappings"),
          textOutput(ns("elem_selected")),
          hr(),
          actionButton("submit", label = "Submit", class = "btn-primary")
        )
      )
    )
  )
}

#' elementControl Server Functions
#'
#' @noRd 
mod_elementControl_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Keep track of selected tab
    tab_selected = reactiveVal("parties") # Default tab: "parties" 
    lapply(vegx_names, function(name){
      observeEvent(input[[name]], tab_selected(name))
    })
    
    # Render UI for selected VegX element
    output$tree = shinyTree::renderTree({
      vegx_to_list(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_selected(), "']"))), "name")
    })

    # Observe Element selection
    elem_selected = reactiveValues() # A container for selected elements per VegX element
    observeEvent(input$tree, {
      selected = shinyTree::get_selected(input$tree, "slices") # Get selected elements
      selected = unlist(names(selected))
      elem_selected[[tab_selected()]] = selected
    })
    
    output$elem_selected = renderPrint(reactiveValuesToList(elem_selected))
  })
}
## To be copied in the UI
# mod_elementControl_ui("elementControl_ui_1")

## To be copied in the server
# mod_elementControl_server("elementControl_ui_1")
