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

#' Build a html table to summarize a set of inputs
#'
#' @param input_names A character vector of names
#' @param input_values The names of 
#'
#' @return a rendered UI element
#' @noRd
render_summary_table = function(labels, values){
  if(length(values) == 0){ # All input values are NULL
    renderText("-")
  } else {
    values[!sapply(values, isTruthy)] = "-"
    renderTable(tibble(labels, values), spacing = "xs", rownames = F, colnames = F, bordered = F)
  }
}
