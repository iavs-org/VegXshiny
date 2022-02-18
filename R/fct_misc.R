# A generic id generator function 
#' @description A closure that is used for storing and incrementing an id value. Each time the function is called, the id is incremented by 1. 
#'
#' @param value The initial value to be incremented 
#' 
#' @return The incremented value
#'
#' @noRd
id_generator = function(value = 0){
  function(){
    value <<-value+1
    return(value)
  }
}

#' Build a table from element mappings
#' @description Builds a (wide) table of node path - node value pairs from existing mappings (as stored in the `elem_mappings` reactiveVal). The number of columns corresponds to the number of mappings. The number of rows depends on the specified data sources: if all mappings point to text or id values, one row (corresponding to one node) will be generated. If there is at least one mapping to a column in user-supplied data, the output will have the equivalent number of rows and text and id values will be reused.
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
#' @param log_path The path of the log file for the current session
#' 
#' @return A data.frame
#'
#' @noRd
#' 
#' @importFrom utils read.table
read_action_log = function(log_path){
  log = read.table(log_path, header = T, colClasses = c("POSIXct", "factor", "character"), strip.white = F)
  log = log[order(nrow(log):1),]
}

#' Create a new log record
#' @description Creates a new log record
#' 
#' @param type The type of the message (e.g. "Session info", "Insertion warning", etc)
#' @param message The message text
#' @param log_path The path of the log file for the current session
#' @param append Parameter passed to write.table()
#' @param col.names Parameter passed to write.table()
#'
#' @return This function is used for its side effects
#'
#' @noRd
#' @importFrom utils write.table
new_action_log_record = function(log_path, type, message, append = T, col.names = F){
  record = data.frame(
    timestamp = as.character(Sys.time()),
    type      = type,
    message   = message
  )
  write.table(record, log_path, append = append, col.names = col.names, row.names = F)
}
