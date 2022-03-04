#' vegxBuilder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shinyWidgets radioGroupButtons

mod_vegxBuilder_ui <- function(id){
  ns <- NS(id)
  tagList(
    navs_pill_list(
      id=ns("sidebar"),
      widths = c(2, 10),
      selected = "Projects",

      nav("AggregateOrganismObservations",  mod_nodeEditor_ui(ns("aggregateOrganismObservations"))),
      nav("Attributes",                     mod_nodeEditor_ui(ns("attributes"))),
      nav("CommunityConcepts",              mod_nodeEditor_ui(ns("communityConcepts"))),
      nav("CommunityDeterminations",        mod_nodeEditor_ui(ns("communityDeterminations"))),
      nav("CommunityObservations",          mod_nodeEditor_ui(ns("communityObservations"))),
      nav("IndividualOrganismObservations", mod_nodeEditor_ui(ns("individualOrganismObservations"))),
      nav("IndividualOrganisms",            mod_nodeEditor_ui(ns("individualOrganisms"))),
      nav("LiteratureCitations",            mod_nodeEditor_ui(ns("literatureCitations"))),
      nav("Methods",                        mod_nodeEditor_ui(ns("methods"))),
      nav("Note",                           mod_nodeEditor_ui(ns("note"))),
      nav("ObservationGroupings",           mod_nodeEditor_ui(ns("observationGroupings"))),
      nav("OrganismIdentities",             mod_nodeEditor_ui(ns("organismIdentities"))),
      nav("OrganismNames",                  mod_nodeEditor_ui(ns("organismNames"))),
      nav("Parties",                        mod_nodeEditor_ui(ns("parties"))),
      nav("PlotObservations",               mod_nodeEditor_ui(ns("plotObservations"))),
      nav("Plots",                          mod_nodeEditor_ui(ns("plots"))),
      nav("Projects",                       mod_nodeEditor_ui(ns("projects"))),
      nav("Protocols",                      mod_nodeEditor_ui(ns("protocols"))),
      nav("SiteObservations",               mod_nodeEditor_ui(ns("siteObservations"))),
      nav("Strata",                         mod_nodeEditor_ui(ns("strata"))),
      nav("StratumObservations",            mod_nodeEditor_ui(ns("stratumObservations"))),
      nav("SurfaceCoverObservations",       mod_nodeEditor_ui(ns("surfaceCoverObservations"))),
      nav("SurfaceTypes",                   mod_nodeEditor_ui(ns("surfaceTypes"))),
      nav("TaxonConcepts",                  mod_nodeEditor_ui(ns("taxonConcepts"))),
      nav("TaxonDeterminations" ,           mod_nodeEditor_ui(ns("taxonDeterminations")))        
    )
  )
}

#' vegxBuilder Server Functions
#'
#' @noRd 
mod_vegxBuilder_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define "global" reactive values
    elem_selected = reactiveValues() # Container for selected sub-elements per main element
    elem_mappings = reactiveValues() # Container for existing mappings  

    # Call modules
    lapply(vegx_main_elements, function(tab_name){
      mod_nodeEditor_server(id = tab_name, user_data, tab_name, elem_selected, elem_mappings, vegx_schema, vegx_doc, vegx_txt, action_log, log_path)
    })
  })
}
