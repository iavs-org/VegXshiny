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
    style = "max-width: 1200px;",
    tagList(
      tags$h2("Overview"),
      tags$p("VegXshiny has been developed to make the conversion of vegetation data into well-defined VegX documents as easy as possible. The graphical user interface of VegXshiny helps users who have little experience with programming or markup languages to implement best practices
                      in terms of data management and interoperability."),
      tags$p("At the core of the application is a dynamic import dialog that guides users through the process of mapping their data to the corresponding VegX elements, while ensuring that the generated XML conforms 
                      to the VegX standard. Additionally, ..."),
      
      tags$h3("File Manager"),
      tags$p("The File Manager is the single entry point for user-supplied data. ", tags$b("All files that contain information intended for import need to be uploaded here. "), 
             "Currently, the following file types are supported: ", tags$b("csv, txt, tsv, xls xslx"), " (tabular data) and ", tags$b("xml (TurboVeg and VegX).")),
      tags$p("Note that the File Manager performs only very basic checks on data formatting and validity. While corrupted or malformatted files are usually caught during the import process, 
                      it is always a good idea to double-check files after upload. To this end, the File Manager offers functionality to view, edit and delete uploaded files. Clicking on one
                      of the file icons under 'Uploaded files' will open an editor for the selected file in the 'File Editor' pane. Depending on the file type, this may either be a text editor (xml) or a 
                      spreadsheet editor (tabular data)."),
      
      tags$h3("VegX Import"),
      tags$p("VegXshiny supports the import from tabular data, TurboVeg XML and VegX. Each import option "),
      tags$h4("From Tables"),
      tags$p("Table Format!"),
      tags$h4("From TurboVeg"),
      tags$h4("From VegX"),
      
      tags$h3("XML Viewer"),
      tags$p("The XML Viewer previews the current VegX document. Clicking the 'Edit' button enters the edit mode, where users can modify the raw XML of the VegX document. Caution needs to be taken here, 
                      as manually editing a VegX file can quickly invalidate it. Pressing the 'Validate' button tests whether the current VegX document complies with the VegX XML schema. Potential validation errors will
                      be listed in the Action Log."),
      
      tags$h3("VegX Export"),
      tags$h3("Action Log"),
      tags$h2("FAQ"),
      tags$h4("What is the difference between the VegX R-Package and VegXshiny?"),
      tags$h4("Performance?"),
      
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