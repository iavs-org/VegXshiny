#' aboutVegX UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_aboutVegX_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    style = "max-width: 1200px;",
    tabsetPanel(
      id = ns("about_tabset"),
      tabPanel(
        title = "Overview",
        tags$h1("VegXshiny - A tool to organize plant community data in Veg-X exchange
               format"),
        tags$p("Veg-X (Wiser et al., 2011) is an XML schema that 
               provides both flexibility and precision in representing 
               vegetation data of different origins and formats."), 
        
        tags$p("The Veg-X R package (De Cáceres, 2018) allows to 
               import, integrate, and export vegetation data using the Veg-X 
               standard. Based on this, VegXshiny was developed as a GUI-based 
               application for creating Veg-X documents from small to medium
               sized datasets."),
        
        tags$p("The figure below shows the typical workflow. The core is the 
               conversion part, where dialogs guide the user through the process 
               of mapping data to the appropriate Veg-X elements. VegXshiny can
               also be used for validating existing Veg-X documents."),
        
        tags$div(style = "text-align: left",
                 tags$img(src='www/images/flowchart.svg', 
                          align = "center", width = 400)
        ),

        tags$p("-------------"),
        tags$h2("Acknowledgments"),
        tags$p("The development of VegXshiny is endorsed by the ", 
               tags$a("International Association for Vegetation Science (IAVS)",
                      href = "http://iavs.org/", target = "_blank"),
               "and received funding by the German Research Foundation (DFG, 
               project number 460840087)."),
        
        tags$p("Package development: Christian K\u00f6nig, Sebastian Schmidtlein"),
        
        tags$p("Veg-X standard development: Brad Boyle, Miquel De C\u00e1ceres, 
                Martin Kleikamp, Christian K\u00f6nig, Robert K. Peet, Sebastian 
                Schmidtlein, Nick Spencer, Susan K. Wiser"),
        tags$h2("References"),
        tags$p("Wiser, S.K., Spencer, N., de C\u00e1ceres, M., Kleikamp, M., 
               Boyle, B., Peet R.K. (2011): Veg-X - an exchange standard 
               for plot-based vegetation data. Journal of Vegetation Science 
               22: 598-609, ",
               tags$a("doi:10.1111/j.1654-1103.2010.01245.x",
                      href = "http://dx.doi.org/10.1111/j.1654-1103.2010.01245.x", # nolint
                      target = "_blank"),
               "."),
        tags$p("De C\u00e1ceres (2018): VegX: Vegetation data in Veg-X. 
               R-package, https://iavs-org.github.io/VegX")
        
      ),
      
      tabPanel(
        title = "Veg-X Schema",
        tags$h1("How Veg-X is structured"),
        
        tags$p("The heart of the Veg-X standard is its XML schema. An XML 
               schema is composed of nested tags (elements) with defined 
               attributes that describe the structure of an xml document.
               Veg-X uses this approach to specify what a valid Veg-X 
               document looks like. The current version of the Veg-X schema 
               defines 27 main elements that capture information about 
               plots, taxa, observations and other aspects of a vegetation 
               data set. Each main element may have a varying number of 
               internal nodes that further demarcate thematic sections. 
               Finally, leaf elements of a Veg-X document contain the data. 
               A comprehensive overview of the Veg-X format can be found ", 
               tags$a("here", 
                      href = "https://iavs-org.github.io/VegX/articles/VegXStandard.html", # nolint
                      target = "_blank"),
               "."),
        tags$p("Below is an interactive tree viewer of the Veg-X schema. You 
               can expand an element by clicking the plus icon (+). Additional 
               information about the schema definition of the element 
               (e.g. data type, number of occurrences, etc.) is displayed when 
               hovering the mouse over it. The search box at the top of the 
               tree allows to search for specific node names throughout the 
               schema."),
        hr(),
        fluidRow(
          column(8,
                 shinyTree::shinyTree(ns("tree"), theme = "proton", 
                                      themeIcons = F, search = T)
          ), 
          div(style = "position:sticky; top:100px; margin-left: auto; 
              margin-right: 0; width: 400px;",
              uiOutput(ns("info_box"))
          )
        )
      ),
      
      tabPanel(
        title = "Tutorial",
        tags$h1("Tutorial"),
        tags$p("The User Interface of VegXshiny has five main components. The 
               functionality of each component is described here."),
        
        tags$ol(
          tags$li(tags$a("Prepare", href = paste0("#", ns("file_manager")))),
          tags$li(tags$a("Transform and validate", href = paste0("#", ns("vegx_import"))),
                  tags$ul(style = "list-style-type: none;",
                          tags$li(tags$a("From Tables", href = paste0("#", ns("import_tables")))),
                          tags$li(tags$a("From Turboveg XML", href = paste0("#", ns("import_turboveg")))),
                          tags$li(tags$a("Load Veg-X", href = paste0("#", ns("import_vegx"))))
                  )
          ),
          tags$li(tags$a("Inspect and edit", href = paste0("#", ns("xml_viewer")))),
          tags$li(tags$a("Collect", href = paste0("#", ns("vegx_export")))),
          tags$li(tags$a("Action Log", href = paste0("#", ns("action_log"))))
        ),
        
        hr(),
        tags$h2("Prepare", id = ns("file_manager")),
        tags$p("This File Manager is the single entry point for user-supplied 
                data. ", tags$span("All files that contain information intended 
                for import need to be uploaded here. ", class = "text-info"), 
                "To upload files, click the upload widget on the left panel 
                and browse your local file system. You can select and upload 
                multiple files at once by pressing [CTRL]. Currently, the 
                following file types are supported: "),
        tags$h4("Tabular data:"),
        tags$ul(
          tags$li(".csv (comma-separated)"),
          tags$li(".txt, .tsv (tab-separated)"),
          tags$li(".xls, .xslx")
        ),
        tags$h4("Turboveg 2:"),
        tags$ul(tags$li("xml")),
        tags$h4("Veg-X:"),
        tags$ul(tags$li("xml")),
        tags$p("If an upload of tabular data fails, check the following 
                possible reasons or fixes:"),
        tags$ul(
          tags$li("Column names are expected in the first line and their 
                  number must match the number of columns."),
          tags$li("If you have a text file and commas as column separators,
                  use '.csv' as file extension. In case of tabs
                  use '.tsv' or '.txt'. Space is not recognized as a delimiter.")
        ),
        tags$p("Completed uploads are listed under 'Uploaded Files' with a 
                corresponding icon for the file type."),
        tags$p("Clicking on one of the file icons under 'Uploaded files' 
                will activate the 'File Editor'. Depending on the file type,
                this may either be a text editor (for xml data) or a 
                spreadsheet editor (for tabular data). Click on the 'Edit' 
                tab above the data view to access functions for editing and
                reshaping the uploaded files. The following functions are
                available for tables:"),
        tags$ul(
          tags$li("Save edits > Save: Save edits overwriting the current
                object."),
          tags$li("Save edits > Save as: Save edits as a new object."),
          tags$li("Reshape table > Pivot: Transform table from 'wide' to 
                'long' format. Opens an input screen with detailed 
                instructions."),
          tags$li("Reshape table > Transpose: Turn rows into columns"),
          tags$li("Reshape table > Crop: Removal of table sections"),
          tags$li("Reshape table > Merge columns: Merge two columns into one. 
                This can be needed when data points are identified by more 
                than one row and column."),
          tags$li("Reshape table > Split column: Reverse the merging of 
                columns."),
          tags$li("Edit values > Edit colnames: The title says it all."),
          tags$li("Edit values > Row to colnames: Create column names from
                a row."),
          tags$li("Edit values > Column to rownames: Create rownames from
                a column."),
          tags$li("Edit values > Rownames to column: Transform rownames
                into a new column."),
          tags$li("Edit values > Format date: Transform dates into the 
                required format, which is YYYY-MM-DD. For example, if your
                actual values look like 17.12.2023, the complete conversion
                specification is %d.%m.%Y and for 02/21/18 it is %m/%d/%y.
                (the procedure follows standard r rules, frequent symbols
                are %d (day of the month as number), %m (month as
                number), %b (month abbreviated like Jan), %B (Month full name
                like January), %y (Year 2 digit), %Y (Year 4 digit). The
                separators are used verbatim (like ' ' for space."),
          tags$li("Discard edits: Exit the edit mode without saving"),
        ),
        tags$h2("Transform and validate", id = ns("vegx_import")),
        tags$p("This dialog transforms the data objects from the File 
                Manager into Veg-X. The application supports imports from 
                tabular data, Turboveg XML or Veg-X (for further edits). 
                Note that a new Veg-X overwrites a previous one, i.e.
                sequential imports from different source objects into the 
                same Veg-X document are currently not possible."),
        tags$h3("Build Veg-X from Tables", id = ns("import_tables")),
        tags$p("Tabular data for import are expected to be in a format 
                where each observation has its row and each variable has its
                column. One consequence is that a transformation is 
                required for 'header data' following a convention where
                variables such as date of recording or elevation are 
                organized in rows. A valid 'header' table has at least one 
                column with plot names (as far as static plot attributes like
                spatial coordinates are concerned) and one additional column
                with observation dates in case of non-static attributes
                like species cover. This follows the Veg-X
                logic. Due to limitations of the underlying packages, a
                date column needs to be prepared outside this app if it is
                lacking. A valid species table has a column for plot IDs, a
                column for observation date, a column for species names and
                one for the cover or count values. If species aggregation took
                place in layers, an additional column for layers is needed."),
        tags$p("The tools provided in the File Manager help with the          
                transformations. The import function guides step by step
                towards the goal. Details on the expected format of input data
                is available when hovering the info icon next to the dataset
                selection in the table import dialog. Tutorial videos are planned for common tasks."),
        tags$p("The import of tabular data is structured into five steps. 
                Mandatory fields are marked with a star (*)"),
        tags$ol(
          tags$p(tags$li("Project information")),
          tags$p("Provide a general description of the project and its 
                contributors. No files need to be assigned in this step."),
          tags$p(tags$li("Plot information")),
          tags$p("Describe static plot properties (like surface area of the 
                 plot. This is distinguished from features that belong 
                 to a plotObservation, such as the date of a recording). 
                 The expected data set contains one plot per row. Each row is 
                 identified by a unique plot ID (mandatory) and may contain 
                 static plot properties (optional):"),
          tableOutput(ns("example_df_plots")),
          br(),
          tags$p("Each mapped plot property needs to be linked to a 
                measurement method. Prefabricated descriptions of
                common measurement methods are available in the corresponding 
                mapping dialog. For example, if plot area was measured in 
                square metres, selecting \"Plot area/m2\" as measurement 
                method will define an appropriate method and link it to the 
                measurement values during import. If none of the predefined
                methods is applicable, a custom method can be defined by 
                selecting the \"...add custom method\" option."),

          tags$p(tags$li("Observations")),
          tags$p("Add different types of observations, such as species
                 cover, vegetation layers, or soil properties. All observations 
                 in Veg-X refer to a plotObservation, i.e. a sampling event at 
                 a specific plot at a specific point in time. Thus, 
                 ", tags$span("observation data of any type are identified at 
                 least by a unique combination of plot ID and date. ", 
                 class = "text-info"), "Depending on the observation type, 
                 other attributes are required. For example, the import of 
                 measured species cover values (called 
                 aggregateOrganismObservations), require a dataset with at 
                 least four columns as shown in this example: "),
          tableOutput(ns("example_df_obs")),
          br(),
          tags$p("This is an example for a 'long table', - the 
                 species coverages are often stored differently, i.e. a 
                 transformation is often necessary before importing (there 
                 are tools available to help you with that).  If the 
                 aggregateOrganismObservations were made at a more granular 
                 level, e.g. for separate subplots or vegetation strata, 
                 additional data columns are needed."),
          tags$p("As in the case of plot properties, measurements of 
                 observations require defined measurement methods. A dropdown 
                 menu provides a list of predefined methods. If none of these  
                 predefined methods is applicable, a custom method can be 
                 created by selecting the \"...add custom method\" option."),
          tags$p("Note that currently VegXshiny does not support imports into 
                 the Veg-X container for measurements applying to an entire 
                 plant community, such as successional stage (called 
                 communityObservations). The same applies to measurements 
                 related to individual organisms, such as DBH (called 
                 individualOrganismObservations)."),
          tags$p(tags$li("Review inputs")),
          tags$p("Check your mapped inputs before starting the import. 
                 Color-coded boxes indicate the status:"),
          tags$ul(style = "margin-bottom: 10px",
                  tags$li(tags$span("Green", style = "color: #00a97e"), 
                          ": Inputs complete, ready for import."),
                  tags$li(tags$span("Red", style = "color: #da3b3b"), 
                          ": Inputs incomplete, review before import."),
                  tags$li(tags$span("Grey", style = "color: #666666"), 
                          ": Inputs incomplete, ignored during import."),
          ),
          tags$p("Only when all input categories are marked either green or 
                 grey, you will be able to proceed with the import."),
          
          tags$p(tags$li("Import")),
          tags$p("Pressing", tags$i("Import"), "and confirming the dialog will 
                 create a new Veg-X document from the specified mappings. ", 
                 tags$span("Your current Veg-X document will be overwritten by 
                 this. ", class = "text-info")),
          tags$p("Note that the import may take a while when working with large 
                 input files containing thousands of observations. A progress 
                 bar in the lower left corner indicates the current status. 
                 Once the import is finished, you can view the imported Veg-X 
                 file in the ", tags$i("Veg-X viewer"), " or export it under 
                 ", tags$i("Veg-X export."))
        ),
        
        tags$h3("Build Veg-X from Turboveg", id = ns("import_turboveg")),
        tags$p("You can import data from ", 
               tags$a("Turboveg 2 standard XML files", 
               href = "https://www.synbiosys.alterra.nl/turboveg/help/idh_export_xml.htm",  # nolint
               target = "_blank"), ". The process has only two steps:"),
        tags$ol(
          tags$p(tags$li("Read data from Turboveg XML")),
          tags$p("Choose an uploaded Turboveg 2 XML file and prepare the
                 data for import. This step extracts the data from Turboveg XML
                 into an import-ready format. A progress bar in the lower left 
                 corner indicates the current status. Once the extraction is 
                 finished, a summary of the data is displayed."),
          tags$p(tags$li("Import Turboveg data into Veg-X")),
          tags$p("Submitting the pre-processed Turboveg file will open an 
                 import dialog where you can select, which 'undefined' Turboveg 
                 header data you want to import (i.e. data lacking information 
                 that is required by pre-defined Veg-X elements). The selected 
                 fields will be attached as user-defined to their corresponding
                 plot node in the Veg-X document. Pressing 
                 ", tags$i("Proceed"), "will then start the import of the 
                 Turboveg data to Veg-X.")
        ),
        tags$h3("Load Veg-X", id = ns("import_vegx")),
        tags$p("Choose an uploaded Veg-X file for validation. Review the 
               summary and run the import."),
        
        tags$h2("Inspect and edit", id = ns("xml_viewer")),
        tags$p("The Veg-X XML Viewer previews the current Veg-X document. 
               Clicking the 'Edit' button let you enter the edit mode, where 
               you can modify the raw XML of the Veg-X document. Caution needs 
               to be taken here, as manually editing a Veg-X file can quickly 
               invalidate it. Note that edits cannot be undone once they are 
               saved."),
        tags$p("Pressing the 'Validate' button tests whether the current Veg-X
               document complies with the Veg-X XML schema and whether there 
               are potential issues with references among nodes. Validation 
               errors and other issues will be listed in the Action Log."),
        
        tags$h2("Collect", id = ns("vegx_export")),
        tags$p("Veg-X documents can be exported either as Veg-X XML file or as 
               a zipped collection of long-format tables. The latter option may 
               be convenient if data from Veg-X need to be imported into 
               a generic database system. ", tags$span("Note that Turboveg 3 
               supports the import of Veg-X XML documents.",  
               class = "text-info")), 
        
        tags$h2("Action Log", id = ns("action_log")),
        tags$p("The Action Log records user actions and application messages 
               during a session. This includes file uploads and edits, import 
               messages, Veg-X document edits and validations and file exports. 
               If any problem occurrs during the session, the Action Log 
               provides additional information.")
      ),
      
      tabPanel(
        title = "FAQ",
        tags$h1("Frequently asked questions"),
        tags$h3("How to format my data correctly for VegXshiny?", 
                class = "text-info"),
        tags$p("VegXshiny expects tabular data in a format, where each column 
               is a variable and each row is an observation. Details on the 
               expected format of input data is always available when hovering 
               the info icon next to the dataset selection in the table import 
               dialog."),
        tags$p("If your dataset does not meet the format specifications you can 
               use the ", tags$span("Reshape tools in the File Manager", 
                                    class = "text-info"), 
               "or R-functions such as ", 
               tags$code("reshape::melt()"), " or ", 
               tags$code("tidyr::pivot_longer()"), 
               " to organize your data."),

        tags$h3("Why did my import fail?", class = "text-info"),
        tags$p("If the import fails, you may consult the ", 
               tags$i("Action Log"), " for details on why the data could not 
               be imported."),
        
        tags$h3("My files are too large to upload, what should I do?", 
                class = "text-info"),
        tags$p("The online version of VegXshiny supports uploads up to a size 
               of 50 MB. Larger files may lead to a significant slowdown of the 
               server and compromise the responsiveness of the application. If
               you need to handle larger files you can install VegXshiny from 
               github and run the application locally:"),
        tags$code("install.packages(\"devtools\")"), tags$br(),
        tags$code("devtools::install_github(\"iavs-org/VegXshiny\")"), 
        tags$br(),
        tags$code("VegXshiny::run_app(max_upload_size_MB = 99)  # set limit and run"), 
        tags$br(),
        tags$p("Alternatively, you can can write an import script using the", 
               tags$a("VegX R-package", 
                      href = "https://github.com/iavs-org/VegX", 
                      target="_blank"), 
                ".")
      ),
      
      tabPanel(
        title = "Contact",
        tags$h1("Contact"),
        tags$p("For questions and feedback, please open an issue on ",
               tags$a("Github", href = "https://github.com/iavs-org/VegXshiny", 
                      target="_blank"))
      ),
      tabPanel(
        title = "Privacy",
        tags$h1("Privacy"),
        tags$p("All data is deleted at the end of a session. No data is collected. This site does not use cookies.")
      ),
      tabPanel(
      title = "Legals",
      tags$h1("Impressum"),
      tags$h3("Anbieter", class = "text-info"),
      tags$p(HTML("Karlsruher Institut für Technologie (KIT)<br/>
             </br/>
             Rechtlicher Sitz:</br/>
             Kaiserstr. 12<br/>
             76131 Karlsruhe<br/>
             Deutschland<br/>
             Tel. +49 721 608-0<br/>
             Email: info@kit.edu<br/>
             Umsatzsteueridentifikationsnummer: DE266749428<br/>
             <br/>
             Rechtsform: Körperschaft des öffentlichen Rechts</br/>
             Vertretungsberechtigt: Prof. Dr. Oliver Kraft</br/>
             </br/>
             Projektkoordination:</br/>
             Prof. Dr. Sebastian Schmidtlein</br/>
             Institut für Geographie und Geoökologie</br/>
             Kaiserstr. 12</br/>
             76131 Karlsruhe</br/>
             Deutschland</br/>
             Email: schmidtlein@kit.edu</br/>")),
      tags$h3("Links", class = "text-info"),
      tags$p(HTML("Die Web-Seiten des Karlsruher Instituts für Technologie 
             enthalten Verweise (Links) zu Informationsangeboten auf Servern, 
             die nicht der Kontrolle und Verantwortlichkeit des Karlsruher 
             Instituts für Technologie unterliegen. Das Karlsruher Institut für 
             Technologie übernimmt keine Verantwortung und keine Garantie für 
             diese Informationen und billigt oder unterstützt diese auch nicht 
             inhaltlich."))
      ),
    
      
    )
  )
}

#' aboutVegX Server Functions
#'
#' @noRd 
mod_aboutVegX_server <- function(id, vegx_schema, node_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    schema_list = schema_to_list(vegx_schema[[1]], name_attr = "name") 
    
    output$tree = shinyTree::renderTree(schema_list)
    
    output$info_box = renderUI({
      render_node_info(node_info())
    })
    
    output$example_df_plots = renderTable({
      data.frame("plot_identifier" = 1:3, "property_1" = sample(LETTERS, 3), "property_2" = rnorm(3), "..." = "...")
    })
    
    output$example_df_obs = renderTable({
      data.frame("plot_identifier" = c("1","1","1","2","3"), "obs_date" = c("2019-09-23", "2019-09-23", "2019-09-23", "2019-08-02", "2020-07-19"), 
                 "taxon_name" = c("Poa annua", "Bellis perennis", "Trifolium repens", "Picea abies", "Fagus sylvatica"), "cover_measurement" = c(22,5,13,88,69))
    })
  })
}
