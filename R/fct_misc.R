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
#' @param header The header of the output UI
#' @param labels The labels of the output table 
#' @param values The values of the output table
#' @param values_mandatory Indices of values that are mandatory for input group to be valid.
#' @param values_grouping A list with index vectors that indicate the grouping of labels/values (used for validity checking)
#'
#' @return a rendered UI element
#' @noRd
render_summary_table = function(header = NA, labels, values, values_mandatory = NA, values_grouping = list(1:length(values))){
  
  values_truthy = sapply(values, isTruthy)
  div_class = "frame frame-danger"
  if(!any(values_truthy)){ # No input values available
    div_content = renderText("-")
    if(all(is.na(values_mandatory))){div_class = "frame"}
  } else {
    div_content = renderTable(tibble(labels, values), spacing = "xs", rownames = F, colnames = F, bordered = F)
    groups_valid = sapply(values_grouping, function(group_indices){ # Check if values within groups are either all truthy or non-truthy
      return(all(values_truthy[group_indices]) | all(!values_truthy[group_indices]))
    })
    values[!values_truthy] = "-"
    
    if(all(groups_valid) & (all(is.na(values_mandatory)) || all(values_truthy[values_mandatory]))){
      div_class = "frame frame-success"
    } else {
      div_class = "frame frame-danger"
    }
  }
  
  # return div
  div(class = div_class, 
      if(!is.na(header)){h4(header)}, 
      div_content
  )
}