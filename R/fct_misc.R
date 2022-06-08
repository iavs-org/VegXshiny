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

#' Check if user inputs are complete
#'
#' @param values The values of the output table
#' @param values_mandatory Indices of values that are mandatory for input group to be valid.
#' @param values_grouping A list with index vectors that indicate the grouping of labels/values (used for validity checking)
#'
#' @return 1-element logical
#' @noRd
check_input_completeness = function(values, values_mandatory = NA, values_grouping = list(1:length(values))){
  values_truthy = sapply(values, isTruthy)
  if(!any(values_truthy)){ # No input values available
    if(all(is.na(values_mandatory))){
      return(T)
    } 
    return(F)
  } 
  groups_valid = sapply(values_grouping, function(group_indices){ # Check if values within groups are either all truthy or non-truthy
    all(values_truthy[group_indices]) | all(!values_truthy[group_indices])
  })
  
  return(all(groups_valid) & (all(is.na(values_mandatory)) || all(values_truthy[values_mandatory])))
}

#' Build a html table to summarize a set of inputs
#'
#' @param header The header of the output UI
#' @param labels The labels of the output table 
#' @param values The values of the output table
#' @param inputs_complete A logical flag (usually obtained with check_input_completeness)
#'
#' @return a rendered UI element
#' @noRd
render_mapping_summary = function(header = NA, labels, values, inputs_complete){
  values_truthy = sapply(values, isTruthy)
  
  div_class = "frame frame-danger"
  if(!any(values_truthy) && inputs_complete){
    div_class = "frame"
  } else if(inputs_complete){
    div_class = "frame frame-success"
  }
  
  div_content = renderText("-")
  if(any(values_truthy)){
    values[!values_truthy] = "-"
    div_content = renderTable(tibble(labels, values), spacing = "xs", rownames = F, colnames = F, bordered = F)
  }
  
  return(
    div(class = div_class, 
        if(!is.na(header)){h4(header)}, 
        div_content
    )
  )
}

render_export_summary = function(vegx_doc){
  node_summary = lapply(xml_children(vegx_doc), function(node){
    data.frame(
      "VegX Element" = xml_name(node),
      "Node count" = length(xml_children(node)),
      check.names = F
    )
  }) 
  
  if(length(node_summary) == 0){
    return(renderText("VegX document is empty"))
  }
  return(renderTable(bind_rows(node_summary)))
}

#' Convert a VegX xml document to a list of rectangular tables
#'
#' @param vegx_doc the vegx xml-document
#' @param resolve_ids A character vector of VegX main elements, for which ids should be resolved
#'
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' 
#' @return a list of data.frames
#' @noRd
vegx_to_df = function(vegx_doc, resolve_ids = c("attributes", "organismIdentities", "aggregateOrganismObservations")){
  # Convert xml to list of data.frame (1 row per node)
  vegx_list = xml2::as_list(vegx_doc)
  vegx_df = lapply(vegx_list$vegX, function(main_element){
    result = mapply(function(node, node_name){
      values = unlist(node)
      if(is.null(names(values))){ # Set name manually for leaf nodes (e.g. organismName)
        names(values) = node_name
      }
      return(c("id" = attr(node, "id"), values))
    }, node = main_element, names(main_element), SIMPLIFY = F)  
    return(bind_rows(result))
  })
  
  # Process attributes
  if("attributes" %in% resolve_ids && "attributes" %in% names(vegx_df)){
    vegx_df$attributes = vegx_df$attributes %>% 
      pivot_longer(cols = -id, names_sep = "\\.", names_to = c("attributeType", "name")) %>% 
      drop_na() %>% 
      pivot_wider(id_cols = c("id", "attributeType"), names_from = "name", values_from = "value")  
  }
  # Process organisms
  if("organisms" %in% resolve_ids && "organismNames" %in% names(vegx_df) && "organismNames" %in% names(vegx_df)){
    vegx_df$organismIdentities = vegx_df$organismIdentities %>% 
      left_join(vegx_df$organismNames, by = c("originalOrganismNameID" = "id")) %>% 
      select(id, organismName)  
  }
  
  if("aggregateOrganismObservations" %in% resolve_ids && "aggregateOrganismObservations" %in% names(vegx_df)){
    if("stratumObservationID" %in% colnames(vegx_df$aggregateOrganismObservations)){
      vegx_df$aggregateOrganismObservations = vegx_df$aggregateOrganismObservations %>% 
        left_join(vegx_df$plotObservations, by = c("plotObservationID" = "id")) %>% 
        left_join(vegx_df$plots, by = c("plotID" = "id")) %>% 
        left_join(vegx_df$organismIdentities, by = c("organismIdentityID" = "id")) %>% 
        left_join(vegx_df$stratumObservations, by = c("stratumObservationID" = "id")) %>% 
        left_join(vegx_df$strata, by = c("stratumID" = "id")) %>% 
        dplyr::select(id, plotUniqueIdentifier, obsStartDate, organismName, stratumName, measurementValue = aggregateOrganismMeasurement.value)
    } else {
      vegx_df$aggregateOrganismObservations = vegx_df$aggregateOrganismObservations %>% 
        left_join(vegx_df$plotObservations, by = c("plotObservationID" = "id")) %>% 
        left_join(vegx_df$plots, by = c("plotID" = "id")) %>% 
        left_join(vegx_df$strata, by = c("stratumID" = "id")) %>% 
        dplyr::select(id, plotUniqueIdentifier, obsStartDate, organismName, measurementValue = aggregateOrganismMeasurement.value)
    }
  }
  
  return(vegx_df)
  
  
  # TODO Two output options: First - long-table format with potentially resolved ids. 
  #                          Second - vegetation-table with header and cover data (aggOrgObs) or equivalent measurement tables (for indOrgObs, stratumObs, etc.)
}
