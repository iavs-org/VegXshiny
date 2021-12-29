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
#' @importFrom shinyBS bsButton updateButton
#' @importFrom shinyjs toggleState useShinyjs
#' @importFrom stringr str_replace str_extract
#' @importFrom jsonlite fromJSON
#' @importFrom shinyWidgets radioGroupButtons
mod_elementControl_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(width = 2,
                   tagList(
                     tags$label("VegX main elements"),
                     shinyWidgets::radioGroupButtons(ns("input_mode"), choices = c("Basic", "Advanced", "All"), selected = "All",
                                                     justified = TRUE, status = "primary", size = "xs"),
                     uiOutput(ns("control_buttons"))
                   )
      ), 
      mainPanel(
        width = 10,
        fluidPage(
          fluidRow(
            column(12,
                   tags$h2(textOutput(ns("tab_active"))),
                   div(textOutput(ns("annotation")), class = "text-primary annotation"),
                   hr()
            )
          ),
          fluidRow(
            column(12,
                   tags$h3("Elements"),
                   div("Select the VegX elements you want to use.", class = "text-primary annotation"),
                   br(),
                   shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
                   hr()
            )
          ),
          fluidRow(
            column(12,
                   h3("Mappings"),
                   div("Map the selected VegX elements to your uploaded data or provide free text descriptions.", class = "text-primary annotation"),
                   br(),
                   uiOutput(ns("mappings")),
                   hr()
            )
          ),
          fluidRow(
            column(12,
                   actionButton(ns("submit"), label = "Submit", class = "btn-primary"),
            )
          )
        )
      )
    )
  )
}

#' elementControl Server Functions
#'
#' @noRd 
mod_elementControl_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #---------------------------------------------#
    # Control buttons
    #---------------------------------------------#
    elements_show = reactive({
      switch(input$input_mode,
             "Basic" = setdiff(vegx_main_elements, c("complexUserDefined", "literatureCitations", "individualOrganismObservations", "individualOrganisms","note", 
                                                     "observationGroupings", "organismIdentities", "organismNames","protocols", "simpleUserDefined", "strata",
                                                     "stratumObservations", "surfaceCoverobservations", "surfaceTypes", "taxonConcepts", "taxonDeterminations")),
             "Advanced" = setdiff(vegx_main_elements, c("complexUserDefined", "note", "simpleUserDefined", "surfaceCoverobservations", "surfaceTypes", 
                                                        "taxonConcepts", "taxonDeterminations")),
             "All" = vegx_main_elements)
    })
    
    observeEvent(eventExpr = input$input_mode,
                 handlerExpr = {
                   isolate({elements_show = elements_show()})
                   lapply(vegx_main_elements, function(name){
                     shinyjs::toggle(name, condition = name %in% elements_show)
                   })
                 })
    
    output$control_buttons = renderUI(
      lapply(sort(vegx_main_elements), function(name){
        shinyBS::bsButton(inputId = ns(name), 
                          label = stringr::str_replace(name, "^.{1}", toupper),
                          type = "action",
                          block = T, 
                          size = "small",
                          class = "btn-sidebar")
      })
    )
    
    #---------------------------------------------#
    # Tab selection
    #---------------------------------------------#
    tab_last = reactiveVal("parties")
    tab_active = reactiveVal("projects") # Default tab: "projects" 
    output$tab_active = renderText(stringr::str_replace(tab_active(), "^.{1}", toupper))
    lapply(vegx_main_elements, function(name){
      observeEvent(input[[name]], tab_active(name))
    })
    
    observeEvent(eventExpr = tab_active(),
                 handlerExpr = {
                   shinyBS::updateButton(session = session, inputId = ns(tab_active()), style = "class:active;")
                   shinyBS::updateButton(session = session, inputId = ns(tab_last()))
                 })
    
    observeEvent(eventExpr = elements_show(), 
                 handlerExpr = {
                   if(!(tab_active() %in% elements_show())){
                     tab_active("projects")
                   }
                 })
    
    
    # Main elements description
    output$annotation = renderText(
      xml_attr(
        xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_active(), "']"))), "annotation"
      )
    )
    
    # Render UI for selected VegX element
    output$tree = shinyTree::renderTree({
      vegx_to_list(xml_children(xml_find_all(vegx_smpl, paste0(".//*[@name='", tab_active(), "']"))), "name")
    })
    
    #---------------------------------------------#
    # ELEMENT MATCHING 
    #---------------------------------------------#
    # Selected elements
    elem_selected = reactiveValues() # A container for selected elements per VegX main element
    observeEvent(input$tree, {
      selected = shinyTree::get_selected(input$tree, "slices") # Get selected elements
      selected = names(unlist(selected))
      is_leaf = stringr::str_extract(selected, "[^.]+$") %in% vegx_leaf_elements # Check which selected elements are leaf nodes
      elem_selected[[tab_active()]] = selected[is_leaf] # Update elem_selected with selected leaf nodes
    })
    
    # Uploaded Data
    data_columns = reactive({
      sapply(names(user_data), simplify = F, USE.NAMES = T, function(file_name){
        user_data[[file_name]]$x$rColHeaders
      })
    })
    
    # Build Matching UI
    output$mappings = renderUI({
      fluidPage(
        fluidRow(align = "center", 
                 column(5, tags$label("VegX Element")),
                 column(5, tags$label("Data column")),
                 column(1),
                 column(1, tags$label("Enter text"))
        ),
        
        fluidRow(column(5, selectInput(ns("vegx_element"), label = NULL, width = "100%", choices = elem_selected[[tab_active()]])),
                 column(5, selectInput(ns("data_column"),  label = NULL, width = "100%", choices = data_columns())),
                 column(1, 
                        actionButton(ns("add_mapping"), label = NULL, icon = icon("check"), width = "34px", style = "height:34px; padding:0"),
                        actionButton(ns("remove_mapping"), label = NULL, icon = icon("times"), width = "34px", style = "height:34px; padding:0")
                 ),
                 column(1, shinyWidgets::materialSwitch("toggle_input"))
                 
        )
      )
    })
    
    observeEvent(eventExpr = input$add_mapping,
                 handlerExpr = {
                   if(input$vegx_element != "" & input$data_column != ""){
                     toggleState("vegx_element")
                     toggleState("data_column")  
                   } else {
                     showNotification("Please select a VegX element and the corresponding data.", type = "warning")
                   }
                   
                 })
    
    observeEvent(eventExpr = input$remove_mapping,
                 handlerExpr = {
                   removeUI("vegx_element")
                   removeUI("data_column")
                 })
    
    # TODO 
    observeEvent(eventExpr = input$submit,
                 # --> produce xml and add to VegX document
                 # --> update action log and progress tab 
                 handlerExpr = {
                   # Update Style
                   shinyBS::updateButton(session = session, inputId = ns(tab_active()), icon = icon("check", class = "icon-padded"), style = "success")
                   # Select new tab
                   tab_active(elements_show()[which(elements_show() == tab_active()) + 1]) # TODO - fix
                 })
    
  })
}
## To be copied in the UI
# mod_elementControl_ui("elementControl_ui_1")

## To be copied in the server
# mod_elementControl_server("elementControl_ui_1")
