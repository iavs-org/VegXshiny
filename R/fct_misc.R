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
#' @param inputs_complete A logical flag (usually obtained with check_input_completeness())
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

#' Render a summary of the current vegX document
#'
#' @param vegx_doc VegX xml-document
#'
#' @return a rendered summary of the VegX document
#' @noRd
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

render_node_info = function(node_info){
  if(length(node_info) == 0){
    div()
  } else {
    lineage = isolate(node_info$node_lineage)
    attrs = isolate(node_info$node_attributes)
    tagList(
      tags$label("Node summary"),
      tags$p(paste0(lineage, collapse = " > "), style = "color:grey; font-size: 14px"),
      tags$p(attrs$annotation, class = "text-info"),
      tags$label("Properties"),
      renderTable(
        colnames = F,
        data.frame(
          "property" = names(attrs),
          "value" = unlist(attrs)
        ) %>% dplyr::filter(!property %in% c("annotation", "name"))
      )
    )
  }
}

#' Convert a VegX xml document to a list of rectangular tables
#'
#' @param vegx_doc the vegx xml-document
#' @param return_vegtable A flag indicating whether to return a formatted vegetation table
#'
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @import dplyr
#' 
#' @return a list of data.frames
#' @noRd
vegx_to_df = function(vegx_doc, return_vegtable = F){
  
  ##### Convert XML to list of data.frame (1 row per node)
  vegx_list = xml2::as_list(vegx_doc)
  vegx_dfs = lapply(vegx_list$vegX, function(main_element){
    result = mapply(function(node, node_name){
      values = unlist(node)
      if(is.null(names(values))){ # Set name manually for leaf nodes (e.g. organismName)
        names(values) = node_name
      }
      return(c("id" = attr(node, "id"), values))
    }, node = main_element, names(main_element), SIMPLIFY = F)  
    return(bind_rows(result))
  })
  
  ##### Resolve IDs
  # Process attributes
  if("attributes" %in% names(vegx_dfs)){
    vegx_dfs$attributes = vegx_dfs$attributes %>% 
      pivot_longer(cols = -id, names_sep = "\\.", names_to = c("attributeType", "name")) %>% 
      drop_na() %>% 
      pivot_wider(id_cols = c("id", "attributeType"), names_from = "name", values_from = "value")  
  }
  
  # Process organisms
  if("organismNames" %in% names(vegx_dfs) && "organismIdentities" %in% names(vegx_dfs)){
    vegx_dfs$organismIdentities = vegx_dfs$organismIdentities %>% 
      left_join(vegx_dfs$organismNames, by = c("originalOrganismNameID" = "id")) %>% 
      select(id, organismName)  
    
    vegx_dfs$organismNames = NULL
  }
  
  # Process organisms
  if("aggregateOrganismObservations" %in% names(vegx_dfs)){
    if("stratumObservationID" %in% colnames(vegx_dfs$aggregateOrganismObservations)){
      vegx_dfs$aggregateOrganismObservations = vegx_dfs$aggregateOrganismObservations %>% 
        left_join(vegx_dfs$plotObservations, by = c("plotObservationID" = "id")) %>% 
        left_join(vegx_dfs$plots, by = c("plotID" = "id")) %>% 
        left_join(vegx_dfs$organismIdentities, by = c("organismIdentityID" = "id")) %>% 
        left_join(vegx_dfs$stratumObservations, by = c("stratumObservationID" = "id")) %>% 
        left_join(vegx_dfs$strata, by = c("stratumID" = "id")) %>% 
        dplyr::select(id, plotUniqueIdentifier, obsStartDate, organismName, stratumName, measurementValue = aggregateOrganismMeasurement.value)
    } else {
      vegx_dfs$aggregateOrganismObservations = vegx_dfs$aggregateOrganismObservations %>% 
        left_join(vegx_dfs$plotObservations, by = c("plotObservationID" = "id")) %>% 
        left_join(vegx_dfs$plots, by = c("plotID" = "id")) %>% 
        left_join(vegx_dfs$organismIdentities, by = c("organismIdentityID" = "id")) %>% 
        dplyr::select(id, plotUniqueIdentifier, obsStartDate, organismName, measurementValue = aggregateOrganismMeasurement.value)
    }
  }
  
  if("stratumObservations" %in% names(vegx_dfs)){
    vegx_dfs$stratumObservations = vegx_dfs$stratumObservations %>%
      left_join(vegx_dfs$plotObservations, by = c("plotObservationID" = "id")) %>%
      left_join(vegx_dfs$plots, by = c("plotID" = "id")) %>%
      left_join(vegx_dfs$strata, by = c("stratumID" = "id")) %>%
      dplyr::select(any_of(c("id", "plotUniqueIdentifier", "obsStartDate", stratumName = "stratumMeasurement.value"))) %>%  
      drop_na()
  }
  
  if("surfaceCoverObservations" %in% names(vegx_dfs)){
    vegx_dfs$surfaceCoverObservations = vegx_dfs$surfaceCoverObservations %>%
      left_join(vegx_dfs$plotObservations, by = c("plotObservationID" = "id")) %>%
      left_join(vegx_dfs$plots, by = c("plotID" = "id")) %>%
      left_join(vegx_dfs$surfaceTypes, by = c("surfaceTypeID" = "id")) %>% 
      dplyr::select(id, plotUniqueIdentifier, obsStartDate, surfaceName, measurementValue = stratumMeasurement.value) %>% 
      drop_na()
  }
  
  if(return_vegtable){
    
    # 1. Join everything to plotobservations
    header_df = vegx_dfs$plotObservations %>%                 
      left_join(vegx_dfs$projects, by = c("projectID" = "id")) %>% 
      left_join(vegx_dfs$plots, by = c("plotID" = "id")) %>% 
      dplyr::select(-id, -plotID, -projectID) %>% 
      dplyr::relocate(plotUniqueIdentifier)
    
    if("stratumName" %in% colnames(vegx_dfs$aggregateOrganismObservations)){
      species_df = vegx_dfs$aggregateOrganismObservations %>%  
        arrange(organismName) %>% 
        mutate(organismName = str_replace_all(organismName, " ", "_")) %>% 
        pivot_wider(id_cols = c(plotUniqueIdentifier, obsStartDate), names_from = c(organismName, stratumName), values_from = measurementValue)
      
      vegtable = header_df %>% 
        left_join(species_df, by = c("plotUniqueIdentifier", "obsStartDate")) %>% 
        as.matrix() %>% 
        t()
      
    } else {
      species_df = vegx_dfs$aggregateOrganismObservations %>%     
        arrange(organismName) %>% 
        mutate(organismName = str_replace_all(organismName, " ", "_")) %>% 
        pivot_wider(id_cols = c(plotUniqueIdentifier, obsStartDate), names_from = organismName, values_from = measurementValue) 
      
      vegtable = header_df %>% 
        left_join(species_df, by = c("plotUniqueIdentifier", "obsStartDate")) %>% 
        as.matrix() %>% 
        t()
      
    }
    return(vegtable)
  }
  
  return(vegx_dfs)
}

#' Convert a Turboveg xml document to a list of rectangular tables
#' 
#' @import xml2
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @return a list of dataframes
#'
#' @noRd

tv_to_df = function(tv_xml){
  # ----------------------------------------------------------- #
  # Plot data #
  plot_nodes = tv_xml %>% xml_child("Plots") %>% xml_children()
  withProgress(message = "Reading Turboveg XML", expr = {
    progress_mod  = ceiling(length(plot_nodes)/min(length(plot_nodes), 10))
    progress_incr = 1 / min(length(plot_nodes), 10)
    
    # Header data - Standard records
    header_std_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.25, message = "Header data (standard)")}
      plot_children = plot_nodes[i] %>% xml_children()
      header_children = plot_children[which(xml_name(plot_children) == "header_data")] %>% xml_children()   
      header_children[which(xml_name(header_children) == "standard_record")] %>% xml_attrs() 
    }) %>% 
      bind_rows() %>%
      dplyr::mutate(across(everything(), na_if, "null")) %>% 
      dplyr::mutate(across(everything(), na_if, "")) %>%
      select_if(~!all(is.na(.)))
    
    # Header data - Undefined records
    header_udf_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.25, message = "Header data (undefined)")}
      plot_children = plot_nodes[i] %>% xml_children()
      header_children = plot_children[which(xml_name(plot_children) == "header_data")] %>% xml_children() 
      udf_names = header_children[which(xml_name(header_children) == "udf_record")] %>% xml_attr("name") 
      udf_vals = header_children[which(xml_name(header_children) == "udf_record")] %>% xml_attr("value") 
      bind_rows(setNames(c(xml_attr(plot_nodes[i], "releve_nr"), udf_vals), c("releve_nr", udf_names)))
    }) %>% 
      bind_rows() %>%
      dplyr::mutate(across(everything(), na_if, "null")) %>% 
      dplyr::mutate(across(everything(), na_if, "")) %>%
      select_if(~!all(is.na(.)))
    
    # ----------------------------------------------------------- #
    # Species data 
    species_std_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.4, message = "Species data")}
      plot_children = plot_nodes[i] %>% xml_children()
      species_data_children = plot_children[which(xml_name(plot_children) == "species_data")] %>% xml_children()
      species_children = species_data_children[which(xml_name(species_data_children) == "species")] %>% xml_children()
      standard_records = species_children[which(xml_name(species_children) == "standard_record")] %>% 
        xml_attrs() %>% 
        bind_rows() %>% 
        mutate(releve_nr = xml_attr(plot_nodes[i], "releve_nr"))
    }) %>% 
      bind_rows()
    
    # ----------------------------------------------------------- #
    # Lookup tables #
    lookup_names = tv_xml %>% xml_child("Lookup_tables") %>% xml_children() %>% xml_name()
    
    incProgress(0.05, message = "Lookup tables")
    lookup_dfs = sapply(lookup_names, simplify = FALSE, USE.NAMES = TRUE, FUN = function(name){
      # Select all leaf nodes in current lookup category
      lookup_parent_nodes = tv_xml %>% 
        xml_child("Lookup_tables") %>% 
        xml_child(name) %>% 
        xml_find_all(".//*[not(*)]") %>% 
        xml_parent()
      
      # Extract data
      lookup_df = lapply(seq_along(lookup_parent_nodes), function(i){
        lookup = list()
        nodes = lookup_parent_nodes[[i]] %>% xml_children()
        for(j in seq_along(nodes)){
          node_name = xml_name(nodes[[j]])
          node_text = xml_text(nodes[[j]])
          if(node_text != ""){                 # Special element with text node
            lookup[[node_name]] = node_text  
          } else {                             # Standard turboveg record
            record = nodes[[j]] %>% xml_attrs() %>% as.data.frame.list()
            lookup[["records"]] = append(lookup[["records"]], list(record))
          }  
        }
        lookup$records = bind_rows(lookup$records)
        return(lookup)
      })
      
      return(lookup_df)
    })
  })
  
  tv_import = list(std_header = header_std_records,
                   udf_header = header_udf_records,
                   species = species_std_records,
                   lookup = lookup_dfs)
  return(tv_import)
}