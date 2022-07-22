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
        tags$h1("VegXshiny", style = "text-align: center",  class = "text-info"),
        tags$h4("An interactive web application for vegetation ecologists", style = "text-align: center; color: grey;"),
        
        tags$p("The increasing digitization of research data has motivated data standardization efforts across scientific disciplines. The vegetation ecological community has recognized the need for a standardized 
                exchange format since at least 2003 and published the initial the version of the Veg-X standard in 2008 (Wiser et al., 2011). Veg-X is implemented as an XML schema that provides both flexibility
                and precision in representing vegetation data of different origin and format."), 
        
        tags$p("Despite being community-driven and designed to be ", tags$i("\"maximally compatible with existing standards and databases\""), " (Wiser et al., 2011), Veg-X has not found wide adoption
                among vegetation ecologists to this date. One of the reasons for this is the complexity of the format and the lack of ecoinformatics tools to easily access it. A major step towards improving the 
                accessibility of Veg-X was the publication of the VegX R-package (De C\u00e1ceres, 2018), which provides tools for importing, integrating and exporting vegetation data using the Veg-X standard. 
                While the VegX package allows users to create Veg-X documents using R programming language, interest in a simple GUI-based application for Veg-X remained high."),
        
        tags$div(style = "text-align: center",
                 tags$img(src='www/images/vegxshiny_overview.png', align = "center", width = 900)
        ),
        
        tags$p(tags$span("VegXshiny aims to make the conversion of vegetation data into Veg-X documents as easy as possible.", class = "text-info"), " The graphical user interface of VegXshiny helps users, who have little 
                experience with programming or markup languages, to build valid Veg-X documents and implement best practices in terms of data management and interoperability. Dynamic import dialogs guide 
                users through the process of mapping their data to the corresponding Veg-X elements, while ensuring that the generated XML conforms to the Veg-X standard. The ability to view and edit 
                input files and output VegX documents gives users full control over their data. Finally, an interactive tree viewer of the Veg-X schema helps making the standard accessible and understandable."),
        
        tags$p("The development of VegXshiny is endorsed by the ", tags$a("International Association for Vegetation Science (IAVS)", href = "http://iavs.org/", target = "_blank"), "."),
        
        
        tags$h3("References"),
        tags$p("Wiser, S.K., Spencer, N., de C\u00e1ceres, M., Kleikamp, M., Boyle, B., Peet R.K. (2011): Veg-X - an exchange standard for plot-based vegetation data. Journal of Vegetation Science 22: 598-609."),
        tags$p("De C\u00e1ceres (2018): VegX: Vegetation data in Veg-X. R-package, https://iavs-org.github.io/VegX"),
        
        tags$h3("Package development"),
        tags$ul(tags$li("Christian K\u00f6nig"),
                tags$li("Sebastian Schmidtlein")),
        
        tags$h3("Veg-X standard development:"),
        tags$ul(tags$li("Miquel De C\u00e1ceres"),
                tags$li("Sebastian Schmidtlein"),
                tags$li("Susan K. Wiser"),
                tags$li("Nick Spencer"),
                tags$li("Robert K. Peet"),
                tags$li("Martin Kleikamp"),
                tags$li("Brad Boyle"),
                tags$li("Christian K\u00f6nig"))
      ),
      
      tabPanel(
        title = "Veg-X Schema",
        h1("The Veg-X Schema", class = "text-info"),
        
        tags$p("The heart of the Veg-X standard is its XML schema. An XML schema is composed of nested tags (elements) with defined attributes that describe the structure of an xml document.
                Veg-X uses this approach to specify what a valid Veg-X document looks like. The current version of the Veg-X schema defines 27 main elements that capture information about 
                plots, taxa, observations and other aspects of a vegetation data set. Each main element may have a varying number of internal nodes that further demarcate thematic sections. 
                Finally, leaf elements of a Veg-X document contain the data. A comprehensive overview of the Veg-X format can be found ", 
               tags$a("here", href = "https://iavs-org.github.io/VegX/articles/VegXStandard.html", target = "_blank"), "."),
        tags$p("Below is an interactive tree viewer of the Veg-X schema. You can expand an element by clicking the plus icon (+). Additional information about the schema definition of the element 
                (e.g. data type, number of occurrences, etc.) are displayed when hovering the mouse over it. The search box at the top of the tree allows to search for specific 
                node names throughout the schema."),
        hr(),
        fluidRow(
          column(8,
                 shinyTree::shinyTree(ns("tree"), theme = "proton", themeIcons = F, search = T)
          ), 
          div(style = "position:sticky; top:100px; margin-left: auto; margin-right: 0; width: 400px;",
              uiOutput(ns("info_box"))
          )
        )
      ),
      
      tabPanel(
        title = "Tutorial",
        tags$h1("Tutorial", class = "text-info"),
        tags$p("The User Interface of VegXshiny has five main components. The functionality of each component is described here."),
        
        tags$ol(
          tags$li(tags$a("File Manager", href = paste0("#", ns("file_manager")))),
          tags$li(tags$a("Veg-X Import", href = paste0("#", ns("vegx_import"))),
                  tags$ul(style = "list-style-type: none;",
                          tags$li(tags$a("From Tables", href = paste0("#", ns("import_tables")))),
                          tags$li(tags$a("From Turboveg XML", href = paste0("#", ns("import_turboveg")))),
                          tags$li(tags$a("From Veg-X", href = paste0("#", ns("import_vegx"))))
                  )
          ),
          tags$li(tags$a("XML Viewer", href = paste0("#", ns("xml_viewer")))),
          tags$li(tags$a("Veg-X Export", href = paste0("#", ns("vegx_export")))),
          tags$li(tags$a("Action Log", href = paste0("#", ns("action_log"))))
        ),
        
        hr(),
        tags$h2("File Manager", id = ns("file_manager"), class = "text-info"),
        tags$p("The File Manager is the single entry point for user-supplied data. ", tags$span("All files that contain information intended for import need to be uploaded here. ", class = "text-info"), 
               "To upload files, click the upload widget on the left panel browse your local file system. You can select and upload multiple files at once by pressing [CTRL]. Currently, the following file types are supported: "),
        tags$label("Tabular data:"),
        tags$ul(
          tags$li(".csv (comma-separated)"),
          tags$li(".txt, .tsv (tab-separated)"),
          tags$li(".xls, .xslx")
        ),
        tags$label("Turboveg 2:"),
        tags$ul(tags$li("xml")),
        tags$label("Veg-X:"),
        tags$ul(tags$li("xml")),
        tags$p("Uploads will be listed under 'Uploaded Files' with a corresponding icon for the file type."),
        tags$p("During upload, only very basic checks on data formatting and validity are performed, and it is always a good idea to double-check files after upload. To this end, the File Manager 
                offers functionality to view, edit and delete uploaded files. Clicking on one of the file icons under 'Uploaded files' will open an editor in the 
                'File Editor' pane. Depending on the file type, this may either be a text editor (xml) or a spreadsheet editor (tabular data)."),  # TODO mention column type 
        
        tags$h2("Veg-X Import", id = ns("vegx_import"), class = "text-info"),
        tags$p("The import dialog is the core functionality of Veg-Xshiny. The application supports imports from tabular data, Turboveg XML and. Note that a new import will overwrite the current 
                Veg-X document, i.e. sequential imports from different source files into the same Veg-X document are currently not possible."),
        tags$h3("From Tables", id = ns("import_tables"), class = "text-info"),
        tags$p("Tabular data for import are generelly expected in ", tags$i("tidy"), " format, where each column is a variable and each row is an observation. This means that ", 
               tags$span("certain tabular data formats, for example species-by-sites matrices, need to be converted into tidy format prior to the import.", class = "text-info"),
               "Available tools in R such as ", tags$code("reshape::melt()"), " or ", tags$code("tidyr::pivot_longer()"), " can help with the conversion process."),
        tags$p("The import of tabular data is structured into five steps. Mandatory fields are marked with a star (*)"),
        tags$ol(
          tags$b(tags$li("Project information")),
          tags$p("Provide a general description of the project and its contributors. No files need to be assigned in this step."),
          
          tags$b(tags$li("Plot information")),
          tags$p("Describe static plot properties. The expected data set contains one plot per row. Each row is identified by a unique plot ID (mandatory) and may contain a variable number of
                 static plot properties (optional):"),
          tableOutput(ns("example_df_plots")),
          br(),
          tags$p("Each mapped plot property requires a measurement method. A list of common measurement methods for a given property is available in the corresponding mapping dialog. 
                 For example, if plot area was measured in square metres, selecting \"Plot area/m2\" as measurement method will define appropriate method and attribute nodes and link them to the measurement values
                 during import. If none of the predefined methods is applicable, a custom method can be defined by selecting the \"...add custom method\" option."),
          
          tags$b(tags$li("Observations")),
          tags$p("Map different types of observation data to the corresponding Veg-X elements. All observations in Veg-X refer to a plotObservation, i.e. a sampling event at a specific plot at a 
                 specific point in time. Thus, ", tags$span("observation data of any type are identified at least by a unique combination of plot ID and date. ", class = "text-info"), 
                 "Depending on the observation type, different additional mappings may be required or optionally available. For example, the import of aggregateOrganismObservations, e.g. measured species
                 cover values during a plotObservation, would require a dataset with at least the following four columns."),
          tableOutput(ns("example_df_obs")),
          br(),
          tags$p("If the aggregateOrganismObservations were made at a more granular level, e.g. for separate subplots or vegetation strata, the corresponding mappings to a data column are required as well."),
          tags$p("As in the case of plot properties, the measurement of observations requires a defined measurement method. A dropdown menu provides a list of predefined methods. If none of these  
                  predefined methods is applicable, a custom method can be created by selecting the \"...add custom method\" option."),
          tags$p("Note that currently VegXshiny does not support the import of communityObservations and individualOrganismObservations."),
          
          tags$b(tags$li("Review inputs")),
          tags$p("Check your mapped inputs before starting the import. Color-coded boxes signify the status of the input mappings:"),
          tags$ul(style = "margin-bottom: 10px",
                  tags$li(tags$span("Green", style = "color: #00a97e"), ": Inputs complete, ready for import."),
                  tags$li(tags$span("Red", style = "color: #da3b3b"), ": Inputs incomplete, review before import."),
                  tags$li(tags$span("Grey", style = "color: #666666"), ": Inputs incomplete, ignored during import."),
          ),
          tags$p("Only when all input categories are marked either green or grey, you will be able to proceed with the import."),
          
          tags$b(tags$li("Import")),
          tags$p("Pressing", tags$i("Import"), "and confirming the dialog will create a new VegX document from the specified mappings. ", 
                 tags$span("Your current Veg-X document will be overwritten by this. ", class = "text-info")),
          tags$p("Note that the import may take a while when working with large input files containing thousands of observations. A progress bar in the lower left corner indicates
                 the current status. Once the import is finished, you can view the imported Veg-X file in the ", tags$i("XML viewer"), " or export it under ", tags$i("Veg-X export."))
        ),
        
        tags$h3("From Turboveg", id = ns("import_turboveg"), class = "text-info"),
        tags$p("You can import data from ", tags$a("Turboveg standard XML files", href = "https://www.synbiosys.alterra.nl/turboveg/help/idh_export_xml.htm", target = "_blank"), ". The process has only two steps:"),
        tags$ol(
          tags$b(tags$li("Read data from Turboveg XML")),
          tags$p("Choose an uploaded XML file and prepare the data for the import. This step extracts the data from Turboveg XML into a tabular format and ensures that the supplied file is readable and contains data.
                  A progress bar in the lower left corner indicates the current status. Once the extraction is finished, a summary of the data is displayed."),
          tags$b(tags$li("Import Turboveg data into Veg-X")),
          tags$p("Submitting the pre-processed Turboveg file will open an import dialog where you can select, which undefined Turboveg header data you want to import. Since these data lack a proper definition
                  that could be imported to Veg-X, the selected fields will be attached as simpleUserDefined types to their corresponding plot node in the Veg-X document. Pressing ", tags$i("Proceed"), 
                 "will then start the import of the Turboveg data to Veg-X.")
        ),
        tags$h3("Load Veg-X", id = ns("import_vegx"), class = "text-info"),
        tags$p("The process of loading a Veg-X file is largely analogous to importing Turboveg XML files: pick and validate an uploaded file, review the summary, and run the import. The import of Veg-X files is much 
                faster than the import from TurboVeg XML, since the supplied file can replace the existing Veg-X document ", tags$i("as is"), " after passing some basic validity checks."),
        
        tags$h2("XML Viewer", id = ns("xml_viewer"), class = "text-info"),
        tags$p("The XML Viewer previews the current Veg-X document. Clicking the 'Edit' button enters the edit mode, where you can modify the raw XML of the Veg-X document. Caution needs to be taken here, 
                as manually editing a Veg-X file can quickly invalidate it. Note that edits cannot be undone once they are saved."),
        tags$p("Pressing the 'Validate' button tests whether the current Veg-X document complies with the Veg-X XML schema and whether there are potential issues with 
               references among nodes. Validation errors and other issues will be listed in the Action Log."),
        
        tags$h2("Veg-X Export", id = ns("vegx_export"), class = "text-info"),
        tags$p("Veg-X documents can be exported either as Veg-X XML file or as a zipped collection of long-format tables. The latter option may be convenient if data from Veg-X need to be imported into 
                a generic database system. ", tags$span("Note that Turboveg 3 supports the import of Veg-X XML documents.",  class = "text-info")), 
        
        tags$h2("Action Log", id = ns("action_log"), class = "text-info"),
        tags$p("The Action Log records user actions and application messages during a session. This includes file uploads and edits, import messages, Veg-X document edits and validations and file exports.",
               tags$span("If any problem occurrs during the session, the Action Log provides additional information.",  class = "text-info"))
      ),
      
      tabPanel(
        title = "FAQ",
        tags$h1("Frequently asked questions", class = "text-info"),
        tags$h3("What's the difference between VegXshiny and the VegX R-package?", class = "text-info"),
        tags$p("Both packages were developed for different use cases. While VegXshiny aims to provide a frictionless and intuitive interface to convert vegetation datasets into Veg-X, the VegX R-package 
               is designed as a programmatic tool for interactive scripting and data pipelines. The development of both packages is supported and endorsded by the IAVS. Future efforts may be directed towards an
               integration of {VegXshiny} and {VegX} into a single R-package."),
        
        tags$h3("How to format my data correctly for VegXshiny?", class = "text-info"),
        tags$p("VegXshiny expects tabular data in ", tags$i("tidy"), " format, where each column is a variable and each row is an observation. Specific details on the expected format of 
                input data is always available when hovering the info icon next to dataset selection in the table import dialog."),
        tags$p("If your dataset does not meet the format specifications, as for example a species-by-sites matrix, you can use the ", tags$span("Reshape tool in the File Manager", class = "text-info"), 
               "or R-functions such as ", tags$code("reshape::melt()"), " or ", tags$code("tidyr::pivot_longer()"), " to organize your data."),

        tags$h3("Why did my import fail?", class = "text-info"),
        tags$p("Generally, VegXshiny tries to handly irregular data without failing. If the import fails anyways, you may consult the ", tags$i("Action Log"), " for details on why the data could not be imported."),
        
        tags$h3("My files are too large to upload, what should I do?", class = "text-info"),
        tags$p("The online version of VegXshiny supports uploads up to a size of 50 MB. Larger files may lead to a significant slowdown of the server and compromise the responiseness of the application. If
               you need to handle larger files you can install VegXshiny from github and run the application locally:"),
        tags$code("install.packages(\"devtools\")"), tags$br(),
        tags$code("devtools::install_github(\"iavs-org/VegXshiny\")"), tags$br(),
        tags$code("VegXshiny::run_app(max_upload_size_MB = 99)  # Set max upload limit"), tags$br(),
        tags$p("Alternatively or you can can write an import script using the", tags$a("VegX R-package", href = "https://github.com/iavs-org/VegX", target="_blank"), ".")
      ),
      
      tabPanel(
        title = "Contact",
        tags$h1("Contact", class = "text-info"),
        tags$p("VegXshiny is supported and endorsed by the International Association for Vegetation Science (IAVS). For questions and feedback, please open an issue on ",
               tags$a("Github", href = "https://github.com/iavs-org/VegXshiny", target="_blank"), " or contact sebastian.schmidtlein@kit.edu")
      )
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
                 "taxon_name" = c("Poa annua", "Bellis perennis", "Trifolium repens", "Picea abies", "Fagus sylvaticum"), "cover_measurement" = c(22,5,13,88,69))
    })
  })
}
