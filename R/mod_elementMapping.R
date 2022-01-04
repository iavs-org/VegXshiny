#' elementMapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_elementMapping_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        tags$h2(textOutput(ns("tab_selected"))),
        div(textOutput(ns("annotation")), class = "text-info annotation"),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$h4(tags$b("Elements")),
        div("Select the VegX elements you want to use.", class = "text-info annotation"),
        shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$h4(tags$b("Mappings")),
        div("Map the selected VegX elements to your uploaded data or provide free text descriptions.", class = "text-info annotation"),
        fluidRow(
          align = "center", 
          column(5, tags$label("VegX Element")),
          column(5, tags$label("Data column")),
          column(2)
        ),
        uiOutput(ns("mapping")),
        hr(),
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(ns("submit"), label = "Submit", class = "btn-primary")
      )
    )
  )
}

#' elementMapping Server Functions
#'
#' @noRd 
mod_elementMapping_server <- function(id, user_data, tab_selected, elem_selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Render non-modularized outputs
    output$tab_selected = renderText(stringr::str_replace(tab_selected, "^.{1}", toupper)) # Capitalize first letter
    output$annotation = renderText(xml_attr(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_selected, "']"))), "annotation"))
    
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
    
    # Keep trock of the number of mapping
    n_mappings = reactiveVal(1)
    
    # Uploaded Data
    data_columns = reactive({
      sapply(names(user_data), simplify = F, USE.NAMES = T, function(file_name){
        user_data[[file_name]]$x$rColHeaders
      })
    })
    
    # Initial Mapping UI
    tab = isolate(tab_selected)
    
    output$mapping = renderUI({
      tagList(
        div(id = ns("mapping_1"),
            fluidRow(column(5, selectInput(ns("vegx_element"), label = NULL, width = "100%", choices = elem_selected[[tab_selected]])),
                     column(5, selectInput(ns("data_column"),  label = NULL, width = "100%", choices = data_columns())),
                     column(2,
                            actionButton(ns("add_mapping_1"), label = NULL, icon = icon("check"), width = "34px", style = "height:34px; padding:0"),
                     )
            )
        ),
        div(id = "placeholder")
      )
    })
    
    observeEvent(eventExpr = input$add_mapping_1,
                 handlerExpr = {
                   # if(input$vegx_element != "" & input$data_column != ""){
                   toggleState("vegx_element")
                   toggleState("data_column")
                   
                   n_mappings(n_mappings() + 1)
                   n = isolate(n_mappings())
                   insertUI(selector = "#placeholder",
                            where = "beforeBegin",
                            ui = {
                              div(id = ns(paste0("mapping_", n)),
                                  fluidRow(
                                    column(5, selectInput(ns("vegx_element"), label = NULL, width = "100%", choices = elem_selected[[tab_selected]])),
                                    column(5, selectInput(ns("data_column"),  label = NULL, width = "100%", choices = data_columns())),
                                    column(2,
                                           actionButton(ns(paste0("add_mapping_", n)), label = NULL, icon = icon("check"), width = "34px", style = "height:34px; padding:0"),
                                           actionButton(ns(paste0("remove_mapping_", n)), label = NULL, icon = icon("times"), width = "34px", style = "height:34px; padding:0")
                                    )
                                  )
                              )
                            }
                   )
                   
                   observeEvent(eventExpr = input[[paste0("remove_mapping_", n)]],
                                handlerExpr = {
                                  removeUI(paste0("#", ns("mapping_"), n))
                                })
                   
                   # } else {
                   #   showNotification("Please select a VegX element and the corresponding data.", type = "warning")
                   # }
                 })
    
    observeEvent(eventExpr = input$remove_mapping,
                 handlerExpr = {
                   removeUI("vegx_element")
                   removeUI("data_column")
                 })
  })
}