#' documentCreation UI Function
#'
#' @description Parent module for the "Create a document" tab. Consists of submodules for the element navigation (sidebar), element selection, element mapping, and submission.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shinyWidgets radioGroupButtons

mod_documentCreation_ui <- function(id){
  ns <- NS(id)
  tagList(
    navs_pill_list(
      id=ns("sidebar"),
      widths = c(2, 10),
      selected = "Parties",
      
      nav_item(tags$label("VegX main elements")),
      nav_item(shinyWidgets::radioGroupButtons(ns("input_mode"), choices = c("Basic", "Advanced", "All"), selected = "All",
                                               justified = TRUE, status = "info", size = "xs")),
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
mod_documentCreation_server <- function(id, user_data, vegx_text){
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
    
    # Define "global" reactive values
    elem_selected = reactiveValues() # Container for selected sub-elements per main element
    vegx_mappings = reactiveValues() # Container for existing mappings  
    tabs_visible  = reactiveVal()    # Currently shown tabs
    
    # Update sidebar depending on input_mode
    observeEvent(eventExpr = input$input_mode,
                 handlerExpr = {
                   tabs_hide = tabs_lookup[[input$input_mode]]
                   lapply(tabs_hide, function(tab){bslib::nav_hide("sidebar", stringr::str_replace(tab, "^.{1}", toupper))})
                   
                   tabs_visible(setdiff(vegx_main_elements, tabs_hide))
                   lapply(tabs_visible(), function(tab){bslib::nav_show("sidebar", stringr::str_replace(tab, "^.{1}", toupper))})
                 })
    
    
    # Call modules
    lapply(vegx_main_elements, function(tab_name){
      mod_elementMapping_server(id = tab_name, user_data, tabs_visible, tab_name, elem_selected, vegx_mappings, vegx_text, session)
    })
  })
}