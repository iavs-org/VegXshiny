#' Run the Shiny Application
#'
#' @param max_upload_size_MB The maximum size for file upload in MB
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(
  onStart = function(){},
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  max_upload_size_MB = 50,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(
      max_upload_size_MB = max_upload_size_MB
    )
  )
}
