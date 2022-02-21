#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
mod_about_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2),
      column(8,
             tagList(
               tags$h2("Introduction"),
               tags$p("A primary technical impediment to large-scale sharing of vegetation data is the lack of a recognized international exchange standard for linking 
                      the panoply of tools and database implementations that exist among various organizations and individuals participating in vegetation research."),
               tags$h2("The Veg-X exchange standard"),
               tags$p("The",
                      tags$b("Veg-X exchange standard"),
                      "for plot-based vegetation data (Wiser et al. 2011) is intended to be used to share and merge vegetation-plot data 
                      of different kinds. Veg-X allows for observations of vegetation at both individual plant and aggregated observation levels. 
                      It ensures that observations are fixed to physical sample plots at specific points in space and time, and makes a distinction between the entity of interest 
                      (e.g. an individual tree) and the observational act (i.e. a measurement). The standard supports repeated measurements of both individual organisms and plots, 
                      and enables the connection between the entity observed and the taxonomic concept associated with that observation to be maintained."),
               tags$h3("Detailed documentation of Veg-X"),
               tags$p("Please read the article",
                      tags$a(href="https://iavs-org.github.io/VegX/articles/VegXStandard.html", "The Veg-X exchange standard"),
                      "to learn more about the standard."),
               tags$h3("XML schema"),
               tags$p("Veg-X is written as an",
                      tags$b("XML schema"),
                      ", which is a definition of user-defined tags to structure textual information in order to create self-describing datasets. XML (Extensible Markup Language) 
                      is an open standard, and XML files are both machine and human-readable (they are stored in plain-text ASCII format). Visit folder",
                      tags$a(href="https://github.com/ChrKoenig/VegXshiny/tree/master/inst/extdata/vegxschema", "vegxschema"),
                      "to see the XML schema representation of the standard. This schema should be used to evaluate whether a given Veg-X XML document conforms 
                      to the definitions and data structure of the standard."),
               tags$h2("An R package to use the Veg-X standard"),
               tags$p("A barrier to the use of a standard like Veg-X is its complexity. To make the exchange schema of Veg-X usable by the wider community requires the development 
                      of informatics tools for mapping data from different input formats (e.g. releve tables from different databases, forest inventory data or stem-mapped forest plots) 
                      into Veg-X, mechanisms to create unique identifiers to allow source datasets to be combined, and tools to export data for data analysis and visualisation. 
                      The VegXshiny R-package has been designed for this purpose. It provides a user-friendly interface to import, integrate, harmonize and export vegetation 
                      data using the Veg-X standard."),
               tags$h3("Acknowledgements"),
               tags$p("Developments endorsed and funded by the [International Association for Vegetation Science](http://iavs.org/)"),
               tags$h4("Package development"),
               tags$ul(tags$li("Christian K\\u00f6nig"),
                       tags$li("Sebastian Schmidtlein"),
                       tags$li("Miquel De C\\u00e1ceres")),
               tags$h4("Veg-X standard development:"),
               tags$ul(tags$li("Miquel De C\\u00e1ceres"),
                       tags$li("Sebastian Schmidtlein"),
                       tags$li("Susan K. Wiser"),
                       tags$li("Nick Spencer"),
                       tags$li("Robert K. Peet"),
                       tags$li("Martin Kleikamp"),
                       tags$li("Brad Boyle")),
                       tags$li("K\\u00f6nig"),
             )
      ),
      column(2)
    )
  )
}

#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}