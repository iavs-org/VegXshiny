#' Build n table 
#' @description Builds a (wide) table of node path - node value pairs from existing mappings (as stored in the `vegx_mappings` reactiveVal). The number of columns corresponds to the number of mappings. The number of rows depends on the specified data sources: if all mappings point to text or id values, one row (corresponding to one node) will be generated. If there is at least one mapping to a column in user-supplied data, the output will have the equivalent number of rows and text and id values will be reused.
#'
#' @return A data.frame
#'
#' @noRd
build_node_values_df = function(mappings, user_data){
  node_values = lapply(mappings, function(mapping) return(mapping$value))  # list needed here for processing in next step
  node_sources = sapply(mappings, function(mapping) return(mapping$source))
  
  # Replace 'value' with data column if source is "file"
  for(i in which(node_sources == "File")){
    file_name = str_split(node_values[[i]], "\\$", simplify = T)[1]
    column_name = str_split(node_values[[i]], "\\$", simplify = T)[2]
    upload = user_data[[file_name]]
    upload_df = jsonlite::fromJSON(upload$x$data)
    colnames(upload_df) = upload$x$rColHeaders
    node_values[[i]] = upload_df[,column_name]
  }

  tryCatch({
    return(as.data.frame(node_values, check.names = F))
  }, error = function(e){
    return(NULL)
  })
}

#' Read the action log
#' @description Reads and returns the current action log
#'
#' @return A data.frame
#'
#' @noRd
#' 
#' @importFrom here here
#' @importFrom purrr map_df
#' @importFrom utils read.table
read_action_log = function(token){
  log_path = here::here("inst", "app", "www", "logs", paste0("log_", token, ".csv"))
  read.table(log_path, header = T, colClasses = c("POSIXct", "factor", "character"), strip.white = F) %>% purrr::map_df(rev)
}

#' Create a new log record
#' @description Creates a new log record
#'
#' @return A data.frame
#'
#' @noRd
#' @importFrom here here
#' @importFrom utils write.table
new_action_log_record = function(type, message, token, append = T, col.names = F){
  record = data.frame(
    timestamp = as.character(Sys.time()),
    type      = type,
    message   = message
  )
  log_path = here::here("inst", "app", "www", "logs", paste0("log_", token, ".csv"))
  write.table(record, file = log_path, append = append, col.names = col.names, row.names = F)
}
