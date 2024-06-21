#' actionLog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_actionLog_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tabsetPanel(
      tabPanel(
      title = "Log",
        fluidRow(
          div(
            tags$p("Action log", 
                 class = "text-info annotation", style = "padding-top: 30px; padding-bottom: 15px;")
            , style = "margin-left: 15px;"
          ),  
          column(
            width = 12,
            DT::dataTableOutput(ns("action_log"))
          )
        )
      ),
      tabPanel("Help",
        div(
          class = "content",
          tags$p("Help with the action log", 
               class = "text-info annotation", style = "padding-top: 30px; padding-bottom: 15px;"),
          tags$p("The Action Log records user actions and application messages 
                   during a session. This includes file uploads and edits, import 
                   messages, Veg-X document edits and validations and file exports. 
                   If any problem occurrs during the session, the Action Log 
                   may provide relevant information.")
        ) 
      ) 
    )  
  )
}

#' actionLog Server Functions
#'
#' @noRd 
mod_actionLog_server <- function(id, action_log){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$action_log <- DT::renderDataTable(expr = action_log(), 
                                             selection = 'none',
                                             rownames = F,
                                             escape = F,
                                             options = list(paging=FALSE, columnDefs = list(list(width = '180px', targets = c(0,1)))))
  })
}