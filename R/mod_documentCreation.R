#' documentCreation UI Function
#'
#' @description Parent module for the "Create a document" tab. Consists of submodules for the element navigation (sidebar), element selection, element mapping, and submission.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @import bslib
#' @import shinyjs 
#' @importFrom shinyBS bsButton updateButton
#' @importFrom stringr str_replace str_extract
#' @importFrom shinyWidgets radioGroupButtons

mod_documentCreation_ui <- function(id){
  ns <- NS(id)
  tagList(
    navs_pill_list(
      id=ns("sidebar"),
      widths = c(2, 10),
      selected = "Projects",
      
      nav_item(tags$label("VegX main elements")),
      nav_item(shinyWidgets::radioGroupButtons(ns("input_mode"), choices = c("Basic", "Advanced", "All"), selected = "All",
                                               justified = TRUE, status = "primary", size = "xs")),
      nav_item(hr()),
      
      nav("AggregateOrganismObservations",  mod_elementMapping_ui(ns("aggregateOrganismObservations"))),
      nav("Attributes",                     mod_elementMapping_ui(ns("attributes"))),
      nav("CommunityConcepts",              mod_elementMapping_ui(ns("communityConcepts"))),
      nav("CommunityDeterminations",        mod_elementMapping_ui(ns("communityDeterminations"))),
      nav("CommunityObservations",          mod_elementMapping_ui(ns("communityObservations"))),
      nav("ComplexUserDefined",             h2("not supported")),
      nav("IndividualOrganismObservations", mod_elementMapping_ui(ns("individualOrganismObservations"))),
      nav("IndividualOrganisms",            mod_elementMapping_ui(ns("individualOrganisms"))),
      nav("LiteratureCitations",            mod_elementMapping_ui(ns("literatureCitations"))),
      nav("Methods",                        mod_elementMapping_ui(ns("methods"))),
      nav("Note",                           mod_elementMapping_ui(ns("note"))),
      nav("ObservationGroupings",           mod_elementMapping_ui(ns("observationGroupings"))),
      nav("OrganismIdentities",             mod_elementMapping_ui(ns("organismIdentities"))),
      nav("OrganismNames",                  mod_elementMapping_ui(ns("organismNames"))),
      nav("Parties",                        mod_elementMapping_ui(ns("parties"))),
      nav("PlotObservations",               mod_elementMapping_ui(ns("plotObservations"))),
      nav("Plots",                          mod_elementMapping_ui(ns("plots"))),
      nav("Projects",                       mod_elementMapping_ui(ns("projects"))),
      nav("Protocols",                      mod_elementMapping_ui(ns("protocols"))),
      nav("SimpleUserDefined",              h2("not supported")),
      nav("SiteObservations",               mod_elementMapping_ui(ns("siteObservations"))),
      nav("Strata",                         mod_elementMapping_ui(ns("strata"))),
      nav("StratumObservations",            mod_elementMapping_ui(ns("stratumObservations"))),
      nav("SurfaceCoverObservations",       mod_elementMapping_ui(ns("surfaceCoverObservations"))),
      nav("SurfaceTypes",                   mod_elementMapping_ui(ns("surfaceTypes"))),
      nav("TaxonConcepts",                  mod_elementMapping_ui(ns("taxonConcepts"))),
      nav("TaxonDeterminations" ,           mod_elementMapping_ui(ns("taxonDeterminations")))        
    )
  )
}

#' documentCreation Server Functions
#'
#' @noRd 
mod_documentCreation_server <- function(id, user_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define non-reactive values
    tabs_lookup = list(
      "All" = "",
      "Advanced" = c("complexUserDefined", "note", "simpleUserDefined", "surfaceCoverobservations", "surfaceTypes",
                     "taxonConcepts", "taxonDeterminations"),
      "Basic" = c("complexUserDefined", "literatureCitations", "individualOrganismObservations", "individualOrganisms","note",
                  "observationGroupings", "organismIdentities", "organismNames","protocols", "simpleUserDefined", "strata",
                  "stratumObservations", "surfaceCoverobservations", "surfaceTypes", "taxonConcepts", "taxonDeterminations")
    )
    
    # Update sidebar depending on input_mode
    observeEvent(eventExpr = input$input_mode,
                 handlerExpr = {
                   tabs_hide = tabs_lookup[[input$input_mode]]
                   lapply(tabs_hide, function(tab){bslib::nav_hide("sidebar", stringr::str_replace(tab, "^.{1}", toupper))})
                   
                   tabs_show = setdiff(vegx_main_elements, tabs_hide)
                   lapply(tabs_show, function(tab){bslib::nav_show("sidebar", stringr::str_replace(tab, "^.{1}", toupper))})
                 })
    
    # Define shared reactive values
    elem_selected = reactiveValues() # Container for selected sub-elements per main element
    vegx_mappings = reactiveValues() # Container for existing mappings  
    
    # Call modules
    lapply(vegx_main_elements, function(tab_name){
      mod_elementMapping_server(id = tab_name, user_data = user_data, tab_selected = tab_name, elem_selected = elem_selected)
    })
  })
}