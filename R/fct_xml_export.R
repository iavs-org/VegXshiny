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
  for(i in which(node_sources %in% c("file", "File"))){
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
new_vegx_document = function(){
  schema_files = load_schema()
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
#' @param log_path The path to the sessions temporary log file
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' @importFrom lubridate ymd
#' 
#' @noRd
#' 
#' @return xml_document
new_vegx_node = function(node_paths, node_values, id = NULL, log_path, vegx_schema, write_log = T){ 
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  conditions = list(warnings = c(), errors = c())
  
  if(length(root_name) != 1){
    if(write_log){
      new_action_log_record(log_path, "Insertion error", paste0("Could not add ", root_name, " node. Node paths do not share the same root."))
    }
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
  root_definition = xml_find_all(vegx_schema, paste0(".//*[@name='", root_name, "']"))
  if(!(length(xml_children(root_node)) == 0  & xml_text(root_node) == "") & xml_has_attr(root_definition, "id")){ # ID is required by schema
    generator_name = id_lookup[paste0(root_name,"ID")]
    if(is.null(id)){                      
      id = id_factory[[generator_name]]()  # If id = NULL: generate a new one
    } else {                              
      id_factory[[generator_name]]()       # otherwise: burn one id to keep counter equal to number of added nodes
    }
  }
  xml_set_attr(root_node, "id", id)
  
  # set other attributes of root node
  if(root_name == "organismName"){
    xml_attr(root_node, "taxonName") = "true"     # special case in entire schema --> always set to true
  }
  
  # Create Log entry and return node
  if(write_log){
    if(length(xml_children(root_node)) == 0 & xml_text(root_node) == ""){
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
merge_into_vegx_node = function(target_root, node_paths, node_values, log_path, vegx_schema, write_log = T){ # TODO change param order for consistency, move vegx_schema to end 
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  root_id = xml_attr(target_root, "id")
  conditions = list(warnings = c(), errors = c())
  
  if(length(root_name) != 1){
    if(write_log){
      new_action_log_record(log_path, "Merge error", paste0("Could not merge with ", root_name, " element (id = ", root_id, "). Node paths do not share the same root."))
    }
    return(list(warnings = 0, errors = 1))
  }
  
  # Build XML
  if(length(target_root) == 0){
    if(write_log){
      new_action_log_record(log_path, "Merge error", paste0("Could not merge mappings into ", root_name, " element (id = ", root_id, "). No such element id."))
    }
    return(list(warnings = 0, errors = 1))
  }
  
  withCallingHandlers({
    build_xml(target_root, node_names, node_values, vegx_schema)
  }, warning = function(w){
    conditions$warnings <<- c(conditions$warnings, w$message)
  }, error = function(e){
    conditions$errors <<- c(conditions$errors, e$message)
  })
  
  if(write_log){
    if(length(conditions$warnings) != 0 | length(conditions$errors) != 0){
      warnings = ifelse(length(conditions$warnings) == 0, "", paste0("<li>Warning: ", conditions$warnings, "</li>", collapse = ""))
      errors = ifelse(length(conditions$errors) == 0, "", paste0("<li>Error: ", conditions$errors, "</li>", collapse = ""))
      message = paste0("Merged mappings into ", root_name, " node (id = ", root_id, ") with the following exceptions:<ul>", warnings, errors, "</ul>")
      new_action_log_record(log_path, "Merge warning", message)
    } else {
      new_action_log_record(log_path, "Merge info", paste0("Successfully merged mappings into ", root_name, " (id = ", root_id, ")"))
    }
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
      warning(paste0("Skipped node at '", paste(node_paths[[i]][1], collapse = " > "), "' and descendents: no node value found.")) 
      break  
    } 
    
    parent = root
    is_choice = FALSE
    if(length(node_paths[[i]]) == 1){ # no traversal needed, just set text value of root
      node_xpath = paste0(".//*[@name='", node_paths[[i]], "']")
      if(length(xml_find_all(vegx_schema, node_xpath)) == 0){ # Check if node name is valid
        warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: invalid node name.")) 
        break  
      }
      xml_text(root) = node_values[i]
    } else { # traverse node path and create new nodes
      for(j in 2:length(node_paths[[i]])){
        node_name = node_paths[[i]][j]
        
        # Skip choices
        if(node_name == "choice"){
          is_choice = TRUE # Next element is a choice element, leave parent as is
          next
        }
        
        node_xpath = paste0(".//xsd:element[@name='", paste(node_paths[[i]][1:j], collapse = "']/*[@name='"), "']") %>%
          str_replace_all("\\*\\[@name='choice']", "xsd:choice")
        schema_node = xml_find_all(vegx_schema, node_xpath)
        
        # Check if node name is valid
        if(length(schema_node) == 0){
          warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: invalid node path."))
          break
        }
        
        # Determine insertion position
        siblings = xml_children(parent) %>% xml_name()
        if(is_choice){ # If node is child of a choice element
          choices = schema_node %>%
            xml_parent() %>%
            xml_children %>%
            xml_attr("name")
          
          # Check other choice is present
          if(length(intersect(siblings, choices[choices != node_name])) != 0){
            warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: only one node allowed in this position."))
            break
          }
          # If not, find siblings of choice element instead of siblings of the node
          siblings_schema = schema_node %>%
            xml_parent() %>%
            xml_parent() %>%  # Go up two levels to skip choice element
            xml_children() %>%
            xml_attr("name")
          
          siblings_schema[which(is.na(siblings_schema))] = node_name # This is a bit hacky and will probably insert incorrectly when there are multiple unnamed elements at the same level, but there's no such case atm
          is_choice = FALSE
        } else {
          siblings_schema_nodes = schema_node %>%
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
        if(length(insert_position) == 0){
          insert_position = 0
        }
        if(length(insert_position) > 1){
          warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': Multiple matches in VegX schema."))
        }
        
        # Insert node
        if(j < length(node_paths[[i]])){
          if(!(node_name %in% siblings)){
            xml_add_child(parent, node_name, .where = insert_position)
          }
          parent = xml_child(parent, node_name)
        } else {
          # Get node schema definition
          node_def = schema_node %>%
            xml_attrs() %>%
            unlist()
          
          node_def_names = names(node_def)
          maxOccurs = 1
          if("maxOccurs" %in% node_def_names){maxOccurs = node_def_names["maxOccurs"]}
          type = "xsd:string"
          if("type" %in% node_def_names){type = node_def["type"]}
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
            val = suppressWarnings(lubridate::ymd(val))
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
link_vegx_nodes = function(target_root, target_node_path, linked_node, log_path, vegx_schema, write_log = T){
  node_names = target_node_path %>% str_split(" > ", simplify = T)
  node_names = node_names[node_names != "choice"]
  link_id = xml2::xml_attr(linked_node, "id")
  
  target_position = xml2::xml_find_all(target_root, paste0("//", paste(node_names, collapse = "//")))
  if(length(target_position) == 0){
    merge_into_vegx_node(target_root, target_node_path, link_id, log_path, vegx_schema, write_log)
  } else {
    xml2::xml_set_text(target_position, link_id)
  }
}

#' Build `xml_nodes` from a VegX template data frame
#' @description Creates a link between related VegX `xml_nodes`. The function takes advantage of the fact, that `xml_nodes` in R are stored as external pointers to C objects. That means an established link between two `xml_nodes` will remain intact, even 
#'
#' @param templates_selected A dataframe of VegX node templates. See 
#' @param vegx_schema An `xml_document` of the vegx schema
#' @param log_path The path to the sessions temporary log file
#' 
#' @noRd
#' 
#' @return A nested list of `xml_node` objects grouped by VegX main elements
templates_to_nodes = function(templates_selected, vegx_schema, log_path, write_log = T){                     
  templates_split = templates_selected %>% 
    group_by(template_id) %>% 
    group_split()
  
  node_list = list()
  # loop over template_ids
  for(i in 1:length(templates_split)){
    nodes_split = templates_split[[i]] %>% 
      group_by(node_id) %>% 
      group_split()
    
    new_nodes = list() # Temporarily save new nodes in case they need to be linked
    # loop over node_ids, create new nodes
    for(j in 1:length(nodes_split)){ 
      link_template = nodes_split[[j]] %>% dplyr::filter(str_detect(node_path, "ID$"))
      node_template = anti_join(nodes_split[[j]], link_template, by = "node_path")
      
      new_node = new_vegx_node(node_template$node_path, node_template$node_value, id = NULL, log_path, vegx_schema, write_log = write_log)
      if(nrow(link_template) != 0){ # ID links present --> link corresponding nodes
        for(k in 1:nrow(link_template)){
          link_vegx_nodes(new_node$node, link_template$node_path, new_nodes[[as.numeric(link_template$node_value)]], log_path, vegx_schema, write_log = write_log)
        }
      }
      
      new_nodes[[j]] = new_node$node
      node_list[[unique(node_template$main_element)]] = append(node_list[[unique(node_template$main_element)]], list(new_node))
    }
  }
  return(node_list)
}
  