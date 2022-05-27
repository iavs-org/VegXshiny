#' exportVegx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exportVegx_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    column(
      width = 10, offset = 1,
      fluidRow(
        tags$label("Document summary"),
        uiOutput(ns("summary")),
        hr(),
        downloadButton(ns("export_xml"), label = "Export XML"),
        downloadButton(ns("export_csv"), label = "Export CSV")
      )
    )
  )
}

#' exportVegx Server Functions
#'
#' @noRd 
mod_exportVegx_server <- function(id, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer(id, function(input, output, session){
    observe({
      vegx_txt() # Can't put observer on vegx_doc (not reactive), so use vegx_txt (readctive) instead 
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
      },
      contentType = "text/xml"
    )
    
    output$export_csv = downloadHandler(
      filename = paste0("vegx_export-", Sys.Date(), ".zip"),
      content = function(file) {
        vegx_df_list = vegx_to_df(vegx_doc, resolve_ids = c())
        if(dir.exists(paste0(tempdir(), "/export"))){unlink(paste0(tempdir(), "/export"))}  # Delete old export directory, if it exists
        dir.create(paste0(tempdir(), "/export"))                                            # Create fresh export directory
        for(df_name in names(vegx_df_list)){
          write.csv(vegx_df_list[[df_name]], file = paste0(tempdir(), "/export/", df_name, ".csv"), row.names = F)  # Write files
        }
       zip(file, list.files(paste0(tempdir(), "/export"), full.names = T), flags = "-j")    # Zip csvs
      },
      contentType = "application/zip"
    )
  })
}