#' aboutVegXshiny UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_aboutVegXshiny_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2),
      column(8,
             tagList(
               tags$h2("Overview"),
               tags$p("VegXshiny has been developed to make the conversion of vegetation data into well-defined VegX documents as easy as possible. The graphical user interface of VegXshiny helps users who have little experience with programming or markup languages to implement best practices
                      in terms of data management and interoperability."),
               tags$p("At the core of the application is a dynamic import dialog that guides users through the process of mapping their data to the corresponding VegX elements, while ensuring that the generated XML conforms 
                      to the VegX standard."),
               tags$h3("File Manager"),
               tags$p("The File Manager is the place for uploading and editing data to be converted into VegX. Currently, the following file types are supported: ", 
                      tags$b("csv, txt, tsv, xls xslx"), " and ", tags$b("xml.")),
               tags$p("Note that ", tags$b("the File Manager does not perform any checks on data formatting"), " 
                      or whether your data can be successfully imported into VegX. However, the ", tags$i("File Editor"), " pane lets
                      you review the uploaded data and, if necessary, make changes to your spreadsheets and XML files."),
               tags$h3("VegX Import"),
               tags$p("VegXshiny supports the import from two different data sources."),
               tags$h4("From Tables"),
               tags$p("Table Format!"),
               tags$h4("From TurboVeg"),
               tags$h3("XML Viewer"),
               tags$h3("VegX Export"),
               tags$h3("Action Log"),
               tags$h2("FAQ"),
               tags$h4("What is the difference between the VegX R-Package and VegXshiny?"),
               tags$h4("Performance?"),
               
             )
      ),
      column(2)
    )
  )
}

#' aboutVegXshiny Server Functions
#'
#' @noRd 
mod_aboutVegXshiny_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}