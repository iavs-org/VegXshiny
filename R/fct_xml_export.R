#' Build a dataframe from list of element mappings
#' @description Builds a (wide) table of node path - node value pairs from existing mappings (as e.g. stored in the `elem_mappings` reactiveVal). The number of columns corresponds to the number of mappings. The number of rows depends on the specified data sources: if all mappings point to text or id values, one row (corresponding to one node) will be generated. If there is at least one mapping to a column in user-supplied data, the output will have the equivalent number of rows and text and id values will be reused.
#'
#' @param mappings A named list. Root elements are named after VegX main elements. Each root element contains zero or more 2-element named lists that represent the mappings. The name of each child element is a valid VegX node path, the element `value` holds the corresponding input and the element `source` indicates whether `value` is to be taken literally ("Text") or points to a data column of values ("File").
#' @param user_data `reactiveValues` of uploaded files
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

#' Initialize an empty VegX document
#' @description Initializes an xml document with namespace definitions and a single root node 'vegX'
#'
#' @param schema_files A list with the original (unlinked) vegX schema definition
#' 
#' @import xml2
#' 
#' @noRd
#' 
#' @return xml_document
new_vegx_document = function(schema_files){
  ns_uris = xml_attrs(schema_files$veg)
  ns_uris = ns_uris[names(ns_uris) != "xmlns:xsd"]
  ns_uris["xmlns:xsi"] = "http://www.w3.org/2001/XMLSchema-instance"
  
  vegx_doc = xml_new_root("vegX")
  xml_set_attrs(vegx_doc, ns_uris)
  return(vegx_doc)
}

#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element
#'
#' @param vegx_schema An `xml_document` of the vegx schema
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values A character vector containing the values of the nodes 
#' @param id The id given to the new node. If NULL (default), a new id will be created.
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' @importFrom lubridate ymd
#' 
#' @noRd
#' 
#' @return xml_document
new_vegx_node = function(vegx_schema, node_paths, node_values, id = NULL, log_path){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  conditions = list(warnings = c(), errors = c())
  
  if(length(root_name) != 1){
    new_action_log_record(log_path, "Insertion error", paste0("Could not add ", root_name, " node. Node paths do not share the same root."))
    return(list(node = NULL, warnings = 0, errors = 1))
  }
  
  # Build XML
  target_root = xml_new_root(root_name)
  
  withCallingHandlers({
    build_xml(target_root, node_names, node_values, vegx_schema)
  }, warning = function(w){
    conditions$warnings <<- c(conditions$warnings, w$message)
  }, error = function(e){
    conditions$errors <<- c(conditions$errors, e$message)
  })
  
  # set ID for root node
  root_node = xml_root(target_root)
  root_definition = xml_find_all(vegx_schema, paste0("//*[@name='", root_name, "']"))
  if(xml_has_attr(root_definition, "id")){ # ID is required by schema
    generator_name = id_lookup[paste0(root_name,"ID")]
    if(is.null(id)){                      
      id = id_factory[[generator_name]]()  # If id = NULL: generate a new one
    } else {                              
      id_factory[[generator_name]]()       # otherwise: burn one id to keep counter equal to number of added nodes
    }
  }
  xml_set_attr(root_node, "id", id)
  
  # Create Log entry and return node
  if(length(xml_children(root_node)) == 0){
    warnings = ifelse(length(conditions$warnings) == 0, "", paste0("<li>Warning: ", conditions$warnings, "</li>", collapse = ""))
    errors = ifelse(length(conditions$errors) == 0, "", paste0("<li>Error: ", conditions$errors, "</li>", collapse = ""))
    message = paste0("Could not add ", root_name, " element. No valid content.<br>", warnings, errors)
    new_action_log_record(log_path, "Insertion error", message)
    conditions$errors = c(conditions$error, message)
    root_node = NULL
  } else if(length(conditions$warnings) != 0 | length(conditions$errors) != 0){
    warnings = ifelse(length(conditions$warnings) == 0, "", paste0("<li>Warning: ", conditions$warnings, "</li>", collapse = ""))
    errors = ifelse(length(conditions$errors) == 0, "", paste0("<li>Error: ", conditions$errors, "</li>", collapse = ""))
    message = paste0("New ", root_name, " element (id = ", id, ") added with the following exceptions:<ul>", warnings, errors, "</ul>")
    new_action_log_record(log_path, "Insertion warning", message)
  } else {
    new_action_log_record(log_path, "Insertion info", paste0("New ", root_name, " (id = ", id, ") successfully added."))
  }
  
  return(list(node = root_node, warnings = length(conditions$warnings), errors = length(conditions$errors)))
}

#' Merge mappings into a VegX node
#' @description Merge mappings into an existing VegX node
#'
#' @param vegx_schema An `xml_document` of the vegx schema
#' @param target_root The root of the node the mappings should be merged with
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values A character vector containing the values of the nodes 
#' @param log_path The path to the sessions temporary log file
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
merge_into_vegx_node = function(vegx_schema, target_root, node_paths, node_values, log_path){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  root_id = xml_attr(target_root, "id")
  conditions = list(warnings = c(), errors = c())
  
  if(length(root_name) != 1){
    new_action_log_record(log_path, "Merge error", paste0("Could not merge with ", root_name, " element (id = ", root_id, "). Node paths do not share the same root."))
    return(list(warnings = 0, errors = 1))
  }
  
  # Build XML
  if(length(target_root) == 0){
    new_action_log_record(log_path, "Merge error", paste0("Could not merge mappings into ", root_name, " element (id = ", root_id, "). No such element id."))
    return(list(warnings = 0, errors = 1))
  }
  
  withCallingHandlers({
    build_xml(target_root, node_names, node_values, vegx_schema)
  }, warning = function(w){
    conditions$warnings <<- c(conditions$warnings, w$message)
  }, error = function(e){
    conditions$errors <<- c(conditions$errors, e$message)
  })
  
  if(length(conditions$warnings) != 0 | length(conditions$errors) != 0){
    warnings = ifelse(length(conditions$warnings) == 0, "", paste0("<li>Warning: ", conditions$warnings, "</li>", collapse = ""))
    errors = ifelse(length(conditions$errors) == 0, "", paste0("<li>Error: ", conditions$errors, "</li>", collapse = ""))
    message = paste0("Merged mappings into", root_name, " node (id = ", root_id, ") with the following exceptions:<ul>", warnings, errors, "</ul>")
    new_action_log_record(log_path, "Merge warning", message)
  } else {
    new_action_log_record(log_path, "Merge info", paste0("Successfully merged mappings into ", root_name, " (id = ", root_id, ")"))
  }
  
  return(list(warnings = length(conditions$warnings), errors = length(conditions$errors)))
}

#' Build XML from vegx mappings
#' @description Utility function used by `*_vegx_node()` functions to build xml from node mappings
#'
#' @param xml_root An object of class xml_document, xml_nodeset or xml_node
#' @param node_paths A list of character vectors
#' @param node_values A character vector containing the values of the nodes 
#' 
#' @import dplyr
#' @import xml2
#' @importFrom lubridate ymd
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
build_xml = function(root, node_paths, node_values, vegx_schema){ 
  # Main loop
  for(i in 1:length(node_paths)){
    if(is.na(node_values[i]) | node_values[i] == ""){
      warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: no node value found.")) 
      break  
    } 
    
    parent = root
    is_choice = FALSE
    
    for(j in 2:length(node_paths[[i]])){ # ignore root node
      node_name = node_paths[[i]][j]
      node_xpath = paste0("..//*[@name='", paste(node_paths[[i]][1:j], collapse = "']//*[@name='"), "']") %>% 
        str_replace_all("\\*\\[@name='choice']", "xsd:choice")
      
      # Skip choices
      if(node_name == "choice"){
        is_choice = TRUE # Next element is a choice element, leave parent as is
        next
      }
      
      # Check if node name is valid
      if(length(xml_find_all(vegx_schema, node_xpath)) == 0){
        warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: invalid node name.")) 
        break  
      }
      
      # Determine insertion position
      siblings = xml_children(parent) %>% xml_name()
      if(is_choice){ # If node is child of a choice element
        choices = vegx_schema %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_children %>% 
          xml_attr("name")
        
        # Check other choice is present
        if(length(intersect(siblings, choices[choices != node_name])) != 0){ 
          warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: only one node allowed in this position.")) 
          break  
        }
        # If not, find siblings of choice element instead of siblings of the node
        siblings_schema = vegx_schema %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_parent() %>%  # Go up two levels to skip choice element
          xml_children() %>% 
          xml_attr("name")
        
        siblings_schema[which(is.na(siblings_schema))] = node_name # This is a bit hacky and will probably insert incorrectly when there are multiple unnamed elements at the same level, but there's no such case atm
        is_choice = FALSE    
      } else {
        siblings_schema_nodes = vegx_schema %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_children() 
        
        if("choice" %in% xml_name(siblings_schema_nodes)){
          siblings_schema = siblings_schema_nodes %>% xml_attr("name")
          choices = siblings_schema_nodes[which(xml_name(siblings_schema_nodes) == "choice")] %>% xml_children() %>% xml_attr("name")
          siblings_schema = append(siblings_schema, choices, after = which(is.na(siblings_schema))) 
          siblings_schema = siblings_schema[!is.na(siblings_schema)]
        } else {
          siblings_schema = siblings_schema_nodes %>% xml_attr("name")
        }
      }
      siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, node_name)]
      
      insert_position = which(siblings_ordered == node_name) - 1
      if(length(insert_position) == 0){insert_position = 0}
      
      # Insert node
      if(j < length(node_paths[[i]])){
        if(!(node_name %in% siblings)){
          xml_add_child(parent, node_name, .where = insert_position)                   
        }
        parent = xml_child(parent, node_name)
      } else {
        # Get node schema definition
        node_def = vegx_schema %>% 
          xml_find_all(node_xpath) %>% 
          xml_attrs() %>% 
          unlist()
        
        maxOccurs = ifelse("maxOccurs" %in% names(node_def), names(node_def)["maxOccurs"], "1")
        type = ifelse("type" %in% names(node_def), node_def["type"], "xsd:string")
        val = node_values[i]
        
        # Check if adding new node is allowed      
        if(!is.na(as.numeric(maxOccurs))){ # max_occ not unbounded
          if(length(siblings[siblings == node_name]) >= as.numeric(maxOccurs)){
            warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': maxOccurs reached.")) 
            break
          }
        }
        
        # Do some format checks on node value
        if(type == "xsd:date"){
          val = suppressWarnings(ymd(val))
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid date format."))
            break
          }
        } else if (type == "xsd:decimal"){
          val = suppressWarnings(as.numeric(val))
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid decimal format."))
            break
          }
        } else if (type == "xsd:integer"){
          val = suppressWarnings(as.integer(val))
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid integer format.")) 
            break
          }
        }
        xml_add_child(parent, node_name, as.character(val), .where = insert_position)  
      }
    }
  }
  
  # Cleanup: remove remains of failed insertions
  leaf_nodes = xml_find_all(root, ".//*[not(*)]")
  empty_leaf_nodes = leaf_nodes[xml_text(leaf_nodes) == ""]
  
  if(length(empty_leaf_nodes) != 0){
    for(empty_leaf in empty_leaf_nodes){
      parent = xml_parent(empty_leaf)
      xml_remove(empty_leaf)
      while(length(xml_children(parent)) == 0 & length(xml_parents(parent)) != 0){
        parent_new = xml_parent(parent)
        xml_remove(parent)
        parent = parent_new
      }
    }
  }
}

#' Link VegX nodes
#' @description Creates a link between related VegX nodes
#'
#' @param target_root The root of the target node
#' @param target_node_path The node path to the VegX element in target node that contains a reference
#' @param linked_node The node that is referenced in target_node_path
#' @param vegx_schema An `xml_document` of the vegx schema
#' @param log_path The path to the sessions temporary log file
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
link_vegx_nodes = function(target_root, target_node_path, linked_node, vegx_schema, log_path){
  
  node_names = target_node_path %>% str_split(" > ", simplify = T)
  node_names = node_names[node_names != "choice"]
  link_id = xml2::xml_attr(linked_node, "id")
  
  target_position = xml2::xml_find_all(target_root, paste0("//", paste(node_names, collapse = "//")))
  if(length(target_position) == 0){
    merge_into_vegx_node(vegx_schema, target_root, target_node_path, link_id, log_path)
  } else {
    xml2::xml_set_text(target_root, link_id)
  }
}