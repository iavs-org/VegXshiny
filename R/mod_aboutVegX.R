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
    tabsetPanel(
      id = ns("about_tabset"),
      tabPanel(
        title = "Overview",
        div(
          class = "content",
          tags$h1("VegXshiny"),
          tags$p(tags$b("Veg-X is a standard for the exchange of vegetation data. 
                 VegXshiny is an interactive web application for converting 
                 datasets into Veg-X and back.")),
          
          tags$p("The increasing digitization of research data has motivated 
                 data standardization efforts across scientific disciplines. 
                 For vegetation data, Veg-X (Wiser et al., 2011) is such a 
                 standard. It provides both flexibility and precision in 
                 representing vegetation data of different origins and 
                 formats. It's scope is the exchange of data-sets both between 
                 vegetation scientists and between vegetation scientists and 
                 database operators."), 
          
          tags$p("VegXshiny was developed as a GUI-based application for 
                 creating Veg-X documents and is build around the R package VegX
                 (De C\u00E1ceres, 2018). It also allows the conversion from Veg-X 
                 documents to other formats."),
          
          tags$p("The figure below shows a typical workflow. Start with the 
                 'Upload' tab of the main menu. The second step is the actual 
                 import, where dialogs guide the user through the process of 
                 mapping data to the appropriate Veg-X elements. Once these 
                 steps have been completed, the VegX code can be reviewed and 
                 downloaded."),
          
          tags$div(style = "text-align: left",
                   # Upload link with an image
                   actionLink("link_to_Upload", 
                              label = tags$img(src = 'www/images/Flowchart_01.svg', 
                                               style = "height: 220px;")),
                   # Image parts
                   tags$img(src = 'www/images/Flowchart_02.svg', 
                                               style = "height: 220px;"),
                   tags$img(src = 'www/images/Flowchart_03.svg', 
                                               style = "height: 220px;"),
                   tags$img(src = 'www/images/Flowchart_04.svg', 
                                               style = "height: 220px;")
          ),

          tags$p("The 'Upload', 'Import', 'Review', 'Download' and 'Action Log' 
                 tabs have their own help sections, which describe their 
                 functionality in detail."),
          
          tags$p("-------------"),
          tags$h2("Package development"),
          tags$p("Christian K\u00f6nig, Sebastian Schmidtlein"),
          tags$h2("Acknowledgments"),
          tags$p("The development of VegXshiny is endorsed by the ", 
                 tags$a("International Association for Vegetation Science (IAVS)",
                        href = "https://www.iavs.org",
                        target = "_blank"),
                 "and received funding by the German Research Foundation (DFG, 
                 project number 460840087."),
          tags$p("Development of the ", 
                 tags$a("R package VegX", 
                        href = "https://iavs-org.github.io/VegX", 
                        target = "blank"),
                 ": Miquel De C\u00e1ceres"),
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
                 R-package", tags$a("https://iavs-org.github.io/VegX",
                                    href = "https://iavs-org.github.io/VegX",
                                    target = "_blank"))
        )   
      ),
      
      tabPanel(
        title = "Veg-X Schema",
        div(
          class = "content",
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
          tags$p("Below is an interactive tree viewer of the Veg-X schema. 
                 Expand an element by clicking the plus icon (+). Additional 
                 information about the schema definition of the element 
                 (e.g. data type, number of occurrences, etc.) is displayed when 
                 hovering the mouse over it. The search box at the top of the 
                 tree allows to search for specific node names throughout the 
                 schema."),
          hr(),
          fluidRow(
            column(6,
                   id = "tree-column",
                   shinyTree::shinyTree(ns("tree"), theme = "proton", 
                                        themeIcons = F, search = T)
            ), 
            column(6,
              div(style = "position:sticky; top:100px; margin-left: auto; 
                  margin-right: 0;",
                  uiOutput(ns("info_box"))
              )
            )
          )
        )
      ),
      
      tabPanel(
        title = "FAQ",
        div(
          class = "content",
          tags$h1("Frequently asked questions"),
          
          tags$div(class = "panel-group", id = "accordion", 
            
            # --- Q+A0 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("What is the scope of VegXshiny?", 
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse0")))
                )
              ),
              tags$div(id = ns("collapse0"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "The scope of Veg-X is the exchange of data-sets both between 
                  vegetation scientists and between vegetation scientists and 
                  database operators. VegXshiny can be used to convert smaller 
                  data sets to Veg-X and for the conversion of Veg-X documents 
                  into table formats."
                )
              )
            ),
            
            # ------------
            # --- Q+A1 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("How to get help?", 
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse1")))
                )
              ),
              tags$div(id = ns("collapse1"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "Each main VegXshiny section (Upload, Import, Review, Download, 
                   Action Log) has a help section where the functionality is 
                   described in detail."
                )
              )
            ),
            # ------------

          
            # --- Q+A2 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("What kind of vegetation data can be converted and 
                          which cannot?",
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse2")))
                )
              ),
              tags$div(id = ns("collapse2"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "Veg-X is extremely flexible in accepting all kinds of 
                  vegetation data, but VegXshiny does not cover everything. 
                  Possible is, for example, the import of records of species in 
                  plots (e.g. species cover in layers) together with ancillary 
                  information about plots and observations. Currently 
                  unsupported is plotless sampling. The plan is to add other 
                  Veg-X elements little by little. If you need a feature 
                  urgently, you can post a request on", 
                  tags$a("Github", href = 
                    "https://github.com/iavs-org/VegXshiny", target="_blank"),
                  ".")
              )
          ),
          # ------------

                      # --- Q+A2 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("Do I need to pre-format my files to use VegXshiny?",
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse3")))
                )
              ),
              tags$div(id = ns("collapse3"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "Turboveg files, and of course Veg-X itself, 
                  should not need further preparation to be imported. Tabular 
                  data must often be rearranged but this can be done in the 
                  'File editor' in the 'Upload' section. Tables need to be in 
                  'long' table format and records need to be uniquely assigned 
                  by plot IDs; time-dependent plot observations (like species 
                  cover) also need a date.")
              )
          ),
            # ------------
        
          
            # --- Q+A3 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("Why did my upload fail?", 
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse4")))
                )
              ),
              tags$div(id = ns("collapse4"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "Check the log. It can sometimes provide useful details as to 
                  why the data could not be imported. Typical import problems 
                  are treated in the dedicated help section"
                )
              )
            ),
            # ------------
          

            # --- Q+A4 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("My files are too large to upload, what can I do?", 
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse5")))
                )
              ),
              tags$div(id = ns("collapse5"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "The online version of VegXshiny currently supports uploads up 
                  to a size of 50 MB. If you are handling larger files you can 
                  install VegXshiny from github and run the application 
                  locally:"),
          
                tags$style(
                  HTML(".code-block {
                          margin: 10px; 
                        }
                      ")
                ),
                tags$div(
                  class = "code-block",
                  tags$code("install.packages(\"devtools\")"), tags$br(),
                  tags$code("devtools::install_github(\"iavs-org/VegXshiny\")"), tags$br(),
                  tags$code("VegXshiny::run_app(max_upload_size_MB = 99)  # set limit and run")
                )
              )
            ),
            # ------------
          
            # --- Q+A5 ---
            tags$div(class = "panel panel-default",
              tags$div(class = "panel-heading",
                tags$h4(class = "panel-title",
                  tags$a("Can I install VegXshiny locally?", 
                         `data-toggle` = "collapse", 
                         `data-parent` = "#accordion", 
                         href = paste0("#", ns("collapse6")))
                )
              ),
              tags$div(id = ns("collapse6"), class = "panel-collapse collapse",
                tags$div(class = "panel-body",
                  "Use the following code to install VegXshiny from github and 
                  run the application:"),
          
                tags$style(
                  HTML(".code-block {
                          margin: 10px; 
                        }
                      ")
                ),
                tags$div(
                  class = "code-block",
                  tags$code("install.packages(\"devtools\")"), tags$br(),
                  tags$code("devtools::install_github(\"iavs-org/VegXshiny\")"), tags$br(),
                  tags$code("VegXshiny::run_app(max_upload_size_MB = 99)  # set limit and run")
                )
              )
            )
            # ------------
          
          )
        )
      ),

      tabPanel(
        title = "Contact",
        tags$h1("Contact"),
        div(
          class = "content",
          tags$p("For questions and feedback, please open an issue on ",
               tags$a("Github", href = "https://github.com/iavs-org/VegXshiny", 
                      target="_blank"))
        )
      ),
      
      tabPanel(
        title = "Privacy",
        div(
          class = "content",
          tags$h1("Privacy"),
          tags$p("All uploaded data is deleted at the end of a session. No data
                 is collected and no cookies used.")
        )
      ),
      
      tabPanel(
      title = "Legals",
      div(
        class = "content",
        tags$h1("Impressum"),
        tags$p("Anbieter:"),
        tags$p(HTML("Karlsruher Institut f\u00FCr Technologie (KIT)<br/>
               </br/>
               Rechtlicher Sitz:</br/>
               Kaiserstr. 12<br/>
               76131 Karlsruhe<br/>
               Deutschland<br/>
               Tel. +49 721 608-0<br/>
               Email: info\u0040kit.edu<br/>
               Umsatzsteueridentifikationsnummer: DE266749428<br/>
               <br/>
               Rechtsform: K\u00F6rperschaft des \u00F6ffentlichen Rechts</br/>
               Vertretungsberechtigt: Prof. Dr. Oliver Kraft</br/>
               </br/>
               Projektkoordination:</br/>
               Prof. Dr. Sebastian Schmidtlein</br/>
               Institut f\u00FCr Geographie und Geo\u00F6kologie</br/>
               Kaiserstr. 12</br/>
               76131 Karlsruhe</br/>
               Deutschland</br/>
               Email: schmidtlein\u0040kit.edu</br/>")),
        tags$p("Links:"),
        tags$p(HTML("Die Web-Seiten des Karlsruher Instituts f\u00FCr Technologie 
               enthalten Verweise (Links) zu Informationsangeboten auf Servern, 
               die nicht der Kontrolle und Verantwortlichkeit des Karlsruher 
               Instituts f\u00FCr Technologie unterliegen. Das Karlsruher 
               Institut f\u00FCr Technologie \u00FCbernimmt keine Verantwortung 
               und keine Garantie f\u00FCr diese Informationen und billigt oder 
               unterst\u00FCtzt diese auch nicht inhaltlich."))
        ),
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
                 "taxon_name" = c("Poa annua", "Bellis perennis", "Trifolium repens", "Picea abies", "Fagus sylvatica"), "cover_measurement" = c(22,5,13,88,69))
    })
  })
}
