#' vegxExport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vegxExport_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .custom-row .col-sm-3 {
          margin-top: 0px !important;
          margin-bottom: 0px !important;
        }
      "))
    ),
    tabsetPanel(
      tabPanel("Download",
        tagList(
          tags$h1("Download"),
          fluidRow(
              tags$label("Veg-X document summary"),
              uiOutput(ns("summary")),
              hr(),
              column(3, downloadButton(ns("export_xml"), label = "Veg-X (xml)", style = "width: 100%;"), style = "padding: 5px;"),
              column(3, downloadButton(ns("export_vegtable"), label = "Wide table (csv)", style = "width: 100%;"), style = "padding: 5px;"),
          class = "custom-row"),
          fluidRow(
              column(3, downloadButton(ns("export_csv"), label = "Long tables (zip)", style = "width: 100%;"), style = "padding: 5px;"),
              column(3, downloadButton(ns("export_rds"), label = "R List (rds)", style = "width: 100%;"), style = "padding: 5px;"),
          class = "custom-row")
        )  
      ),
      tabPanel("Help",
        div(
          class = "content",
          tags$h1("Help with downloading"),
          tags$p("If a valid Veg-X document is available, an overview of the 
                   structure (with the number of nodes of different types) is 
                   shown on this page. The Veg-X document can then be downloaded 
                   as:"),
          tags$ul(
            tags$li(tags$span("xml", class = "text-info"), ": Veg-X xml file"),
            tags$li(tags$span("zip", class = "text-info"), ": Set of 
                   'long tables' - a format that can be easily 
                   processed by generic database systems. While these tables are 
                   provided as a zip archive of csv files, the same tables can 
                   also be downloaded as elements of an R list object:"),
            tags$li(tags$span("rds", class = "text-info"), ": R list object. 
                   Use readRDS() to read it into R."),
            tags$li(tags$span("csv", class = "text-info"), ": A 
                    comma-separated 'wide table' or 'vegetation table') for 
                    import into spreadsheet programs."),
          ),
          tags$p(tags$span("Note that Turboveg 3 is able to import Veg-X XML 
                   documents.", class = "text-info")) 
        )         
      )        
    )
  )
}

#' vegxExport Server Functions
#'
#' @noRd 
mod_vegxExport_server <- function(id, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer(id, function(input, output, session){
    observe({
      vegx_txt() # Can't put observer on vegx_doc (not reactive), so use vegx_txt (reactive) instead 
      output$summary = tryCatch({
        render_export_summary(vegx_doc)
      }, error = function(e){
        renderText("No summary available")
      })
    })
    
    output$export_xml = downloadHandler(
      filename = paste0("vegx_export-", Sys.Date(), ".xml"),
      content = function(file) {
        write_xml(vegx_doc, file)
        new_action_log_record(log_path, "File info", paste0("Veg-X document exported as xml file."))
        action_log(read_action_log(log_path))
      },
      contentType = "text/xml"
    )
    
    output$export_csv = downloadHandler(
      filename = paste0("vegx_export-", Sys.Date(), ".zip"),
      content = function(file) {
        vegx_df_list = vegx_to_df(vegx_doc)
        if(dir.exists(file.path(tempdir(), "export"))){ 
          unlink(file.path(tempdir(), "export"))      # Delete old export directory, if it exists
        } else {
          dir.create(paste0(tempdir(), "/export"))      # Create fresh export directory
        }
        for(df_name in names(vegx_df_list)){
          write.csv(vegx_df_list[[df_name]], file = paste0(tempdir(), "/export/", df_name, ".csv"), row.names = F, na = "")  # Write files
        }
        zip(file, list.files(paste0(tempdir(), "/export"), full.names = T), flags = "-j")    # Zip csvs
        new_action_log_record(log_path, "File info", paste0("Veg-X document exported as zip archive."))
        action_log(read_action_log(log_path))
      },
      contentType = "application/zip"
    )
    
    output$export_rds = downloadHandler(
      filename = function() {
        paste0("vegx_export-", Sys.Date(), ".rds")
      },
      content = function(file) {
        tryCatch({
          vegx_df_list = vegx_to_df(vegx_doc)
          saveRDS(vegx_df_list, file)
          new_action_log_record(log_path, "File info", paste0("Veg-X document exported as rds."))
          action_log(read_action_log(log_path))
        }, error = function(e){
          shiny::showNotification("Couldn't export RDS. Please consult the log for more information.", type = "warning")
          new_action_log_record(log_path, "File error", paste0("Export as RDS failed with the following exceptions:<ul><li>", e, "</li></ul>"))
          action_log(read_action_log(log_path))
        })
      },
      contentType = "application/x-rds"  # Adjusted content type for RDS files
    )

    output$export_vegtable = downloadHandler(
      filename = function() { paste0("vegx_export-", Sys.Date(), ".csv")},
      content = function(file) {
        tryCatch({
          vegx_df = vegx_to_df(vegx_doc, return_vegtable = TRUE)
          write.csv(vegx_df, file, row.names = TRUE, na = "") 
          new_action_log_record(log_path, "File info", paste0("Veg-X document exported as wide table."))
          action_log(read_action_log(log_path))
        }, error = function(e) {
          shiny::showNotification("Couldn't build wide table. Please consult the log for more information.", type = "warning")
          new_action_log_record(log_path, "File error", paste0("Export as wide table failed with the following exceptions:<ul><li>", e$message, "</li></ul>"))
          action_log(read_action_log(log_path))
        })
      },
      contentType = "text/csv"
    )
  })
}
