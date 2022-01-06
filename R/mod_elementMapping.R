#' elementMapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyjs click addClass
#' @importFrom bslib nav_select

mod_elementMapping_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    fluidRow(
      column(
        width = 12,
        tags$h2(textOutput(ns("tab_selected"))),
        div(textOutput(ns("annotation")), class = "text-info annotation"),
        hr(.noWS = "after")
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$h4(tags$b("Elements")),
        div("Select the VegX elements you want to use.", class = "text-info annotation"),
        shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
        hr(.noWS = "before")
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
          column(5, tags$label("Value(s)")),
          column(2,
                 column(9, tags$label("Values from"))
          )
        ),
        div(id = ns("placeholder")),
        actionButton(ns("add_mapping"), label = "Add mapping", icon = icon("plus", class = "icon-padded")), 
        hr(.noWS = c("before","after")),
      )
    ),
    fluidRow(
      align = "center",
      column(
        width = 12,
        actionButton(ns("submit"), label = "Submit", width = "300px", class = "btn-primary", style = "font-weight:bold"),
        shinyjs::hidden(actionButton(ns("edit"), label = "Edit", width = "300px", class = "btn-edit", style = "font-weight:bold")))
    )
  )
}

#' elementMapping Server Functions
#'
#' @noRd 
mod_elementMapping_server <- function(id, user_data, tabs_visible, tab_selected, elem_selected, vegx_mappings, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #### Header ####
    output$tab_selected = renderText(stringr::str_replace(tab_selected, "^.{1}", toupper)) # Capitalize first letter
    output$annotation = renderText(xml_attr(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_selected, "']"))), "annotation"))
    
    # --------------------------------------------------------------------------------------- #
    #### VegX element selection ####
    observeEvent(input$tree, {
      selected = shinyTree::get_selected(input$tree, "slices") # Get selected elements
      selected = names(unlist(selected))
      is_leaf = stringr::str_extract(selected, "[^.]+$") %in% vegx_leaf_elements # Check which selected elements are leaf nodes
      elem_selected[[tab_selected]] = selected[is_leaf] # Update elem_selected with selected leaf nodes
      updateSelectInput(inputId = "mapping_1", choices = elem_selected[[tab_selected]])
    })
    
    output$tree = shinyTree::renderTree({
      vegx_to_list(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_selected, "']"))), "name")
    })
    
    # --------------------------------------------------------------------------------------- #
    #### Element Mapping ####
    mapping_id = reactiveVal(1)    # running number for creating unique input widgets
    mapping_count = reactiveVal(0) # Counter for number of rows shown
    fields_used = reactiveValues() # List elements and values currently selected
    
    data_columns = reactive({
      sapply(names(user_data), simplify = F, USE.NAMES = T, function(file_name){
        user_data[[file_name]]$x$rColHeaders
      })
    })
    
    # On startup, add one mapping row
    observeEvent(eventExpr = mapping_count,
                 handlerExpr = shinyjs::click("add_mapping"))
    
    # Add new mapping row
    observeEvent(eventExpr = input$add_mapping,
                 handlerExpr = {
                   if(length(elem_selected[[tab_selected]]) > mapping_count() | mapping_count() == 0){ 
                     tmp_id = paste0("id", mapping_id())
                     mod_rowGenerator_server(id = tmp_id, tab_selected, elem_selected, data_columns, fields_used, vegx_mappings, mapping_count)
                     insertUI(selector = paste0("#", ns("placeholder")),
                              where = "beforeBegin",
                              ui = mod_rowGenerator_ui(ns(tmp_id))
                     )
                     mapping_id(mapping_id() + 1)
                     mapping_count(mapping_count() + 1)
                   } else {
                     shiny::showNotification("Please select more Elements.", type = "warning")
                   }
                 }
    )
    
    # --------------------------------------------------------------------------------------- #
    #### Submit & Edit buttons ####
    observeEvent(eventExpr = input$submit,
                 handlerExpr = {
                   # TODO 
                   # --> Check mappings
                   # --> produce xml and add to VegX document
                   # --> update action log and progress tab 
                   
                   # Update style
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   
                   # Disable UI
                   shinyjs::disable(selector = paste0("div[data-value='", stringr::str_replace(tab_selected, "^.{1}", toupper), "'] > *"))
                   shinyjs::runjs(paste0("$('#", ns("tree"), " li').each( function() {
                                            $('#", ns("tree") ,"').jstree().disable_node(this.id);
                                          })"))
                   
                   # Insert Edit Button
                   shinyjs::hide("submit")
                   shinyjs::enable("edit")
                   shinyjs::show("edit")
                   
                   # Jump to next tab
                   tabs = sort(tabs_visible())
                   tab_index = which(tabs == tab_selected)
                   next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper)
                   nav_select("sidebar", selected = next_tab, session = parent_session)
                 })
    
    observeEvent(eventExpr = input$edit,
                 handlerExpr = {
                   # TODO
                   # supervise updates
                   # change XML
                   
                   # Update style of tab in sidebar
                   shinyjs::removeClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   shinyjs::enable(selector = paste0("div[data-value='", stringr::str_replace(tab_selected, "^.{1}", toupper), "'] > *"))
                   shinyjs::runjs(paste0("$('#", ns("tree"), " li').each( function() {
                                            $('#", ns("tree") ,"').jstree().enable_node(this.id);
                                          })"))
                   shinyjs::hide("edit")
                   shinyjs::show("submit")
                 })
  })
}