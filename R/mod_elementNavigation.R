#' elementNavigation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyBS bsButton

mod_elementNavigation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$label("VegX main elements"),
    shinyWidgets::radioGroupButtons(ns("input_mode"), choices = c("Basic", "Advanced", "All"), selected = "All",
                                    justified = TRUE, status = "primary", size = "xs"),
    uiOutput(ns("sidebar"))
  )
}

#' elementNavigation Server Functions
#'
#' @noRd 
mod_elementNavigation_server <- function(id, tab_selected, elem_selected){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    session$userData$ns_navbar = ns # save namespace to access tabs from other modules

    output$sidebar = renderUI(
      lapply(sort(vegx_main_elements), function(name){
        if(name == "projects"){ # Initial tab, set class to btn-active
          shinyBS::bsButton(inputId = ns(name),
                            label = "Projects",
                            type = "action",
                            block = T,
                            size = "small",
                            class = "btn-sidebar active")
        } else {
          shinyBS::bsButton(inputId = ns(name),
                            label = stringr::str_replace(name, "^.{1}", toupper),
                            type = "action",
                            block = T,
                            size = "small",
                            class = "btn-sidebar")
        }
      }),
    )
    
    # Set up observers for navigation tabs
    lapply(vegx_main_elements, function(name){
      observeEvent(eventExpr = input[[name]],
                   handlerExpr = {
                     tab_selected$last = tab_selected$current
                     shinyjs::removeClass(tab_selected$last, class = "active")
                     tab_selected$current = name
                     shinyjs::addClass(tab_selected$current, class = "active")
                   })
    })
    
    # Define tabs for different input_mode
    tabs_lookup = list(
      "All" = vegx_main_elements,
      "Advanced" = setdiff(vegx_main_elements, c("complexUserDefined", "note", "simpleUserDefined", "surfaceCoverobservations", "surfaceTypes",
                                                 "taxonConcepts", "taxonDeterminations")),
      "Basic" = setdiff(vegx_main_elements, c("complexUserDefined", "literatureCitations", "individualOrganismObservations", "individualOrganisms","note",
                                              "observationGroupings", "organismIdentities", "organismNames","protocols", "simpleUserDefined", "strata",
                                              "stratumObservations", "surfaceCoverobservations", "surfaceTypes", "taxonConcepts", "taxonDeterminations"))
    )
  
    # Update sidebar depending on input_mode
    observeEvent(eventExpr = input$input_mode,
                 handlerExpr = {
                   # Show/hide tabs depending on input mode
                   lapply(vegx_main_elements, function(name){
                     shinyjs::toggle(name, condition = name %in% tabs_lookup[[input$input_mode]])
                   })

                   # Selected tab is invisible --> go to projects
                   if(!(tab_selected$current %in% tabs_lookup[[input$input_mode]])){
                     shinyjs::click("projects")
                   }
                 })
    
    # Return visible tabs
    return(reactive({tabs_lookup[[input$input_mode]]}))
  })
}