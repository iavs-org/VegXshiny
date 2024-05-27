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
    tabsetPanel(
      tabPanel("Download",
        tags$h1("Download"),
        column(
        width = 12,
        fluidRow(
            tags$label("Veg-X document summary"),
            uiOutput(ns("summary")),
            hr(),
            downloadButton(ns("export_xml"), label = "Veg-X"),
            downloadButton(ns("export_csv"), label = "Long tables"),
            downloadButton(ns("export_vegtable"), label = "Wide table")
          )
        )
      ),
      tabPanel("Help",
        div(
          class = "content",
          tags$h1("Help with downloading"),
          tags$p("If a valid Veg-X document is available, an overview of the 
                 structure (with the number of nodes of different types) is 
                 shown on this page. The Veg-X document can now be downloaded 
                 here. It is also possible to download a set of 'long tables' - 
                 a format that can be easily processed by generic database 
                 systems. The third option is to export a flat file (a 
                 comma-separated 'wide table' or 'vegetation table') for generic 
                 import into spreadsheet programs, R, etc. ", tags$span("Note 
                 that Turboveg 3 is able to import Veg-X XML documents.",
                 class = "text-info")), 
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
    
    output$export_vegtable = downloadHandler(
      filename = paste0("vegx_export-", Sys.Date(), ".csv"),
      content = function(file) {
        tryCatch({
          vegx_df = vegx_to_df(vegx_doc, return_vegtable = T)
          write.csv(vegx_df, file, row.names = T, na = "") 
          new_action_log_record(log_path, "File info", paste0("Veg-X document exported as vegetation table."))
          action_log(read_action_log(log_path))
        }, error = function(e){
          shiny::showNotification("Couldn't build vegetation table. Please consult the log for more information.", type = "warning")
          new_action_log_record(log_path, "File error", paste0("Export as vegetation table failed with the following exceptions:<ul><li>", e, "</li></ul>"))
          action_log(read_action_log(log_path))
        })
      },
      contentType = "text/csv"
    )
  })
}
