#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element
#'
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
new_vegx_node = function(node_paths, node_values, id = NULL, session){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  stopifnot("Node paths do not share the same root" = length(root_name) == 1)
  
  # Build XML
  tmp_root = xml_new_root(root_name)
  build_xml(tmp_root, node_names, node_values, session)

  # set ID for root node
  root_node = xml_find_all(tmp_root, ".")
  root_definition = xml_find_all(vegx_schema_simple, paste0("//*[@name='", root_name, "']"))
  if(xml_has_attr(root_definition, "id")){
    if(is.null(id)){
      generator_name = id_lookup[paste0(root_name,"ID")]
      new_id = id_factory[[generator_name]]()
      xml_set_attr(root_node, "id", new_id)
    } else {
      xml_set_attr(root_node, "id", id)
    }
  }
  
  if(length(xml_children(root_node)) == 0){
    return(NULL)
  } else {
    return(root_node)
  }
}

#' Merge mappings into a VegX node
#' @description Merge mappings into an existing VegX node
#'
#' @param target_node_id The id of the node that mappings should be merged with
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values The values of the nodes 
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
merge_into_vegx_node = function(target_node_id, node_paths, node_values, session){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  stopifnot("Node paths do not share the same root" = length(root_name) == 1)

  # Build XML
  tmp_root = xml_find_all(vegx_doc, paste0("//", root_name, "[@id='", target_node_id, "']"))
  build_xml(tmp_root, node_names, node_values, session)
  
  return(NULL)
}

#' Build XML from vegx mappings
#' @description Utility function used by `*_vegx_node()` functions to build xml from node mappings
#'
#' @param xml_root An object of class xml_document, xml_nodeset or xml_node
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values The values of the nodes 
#' 
#' @import dplyr
#' @import xml2
#' @importFrom lubridate ymd
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
build_xml = function(root, node_names, node_values, session){ 
  
  log_path = paste0("inst/app/www/logs/log_", session$token, ".csv")
  for(i in 1:length(node_names)){
    if(is.na(node_values[i]) | node_values[i] == ""){next} # TODO: add to log ...Skip empty mappings
    
    parent = root
    is_choice = FALSE
    for(j in 2:length(node_names[[i]])){ # ignore root node
      node_name = node_names[[i]][j]
      node_xpath = paste0("..//*[@name='", paste(node_names[[i]][1:j], collapse = "']//*[@name='"), "']") %>% 
        str_replace_all("\\*\\[@name='choice']", "xsd:choice")
      
      # Skip choices
      if(node_name == "choice"){
        is_choice = TRUE # Next element is a choice element
        next
      }

      # Determine insertion position
      siblings =  xml_children(parent) %>% xml_name()
      if(is_choice){ # If node is child of a choice element
        # Check if choice option is already present
        choices = vegx_schema_simple %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_children %>% 
          xml_attr("name")
        
        if(length(intersect(choices, siblings)) != 0){ 
          log = new_action_log_record(type = "Insertion error", message = paste0("Skipped node at '", paste(node_names[[i]][1:j], collapse = " > "), "': only one node allowed in this position.")) 
          break  
        }
        # If not, find siblings of choice element instead of siblings of the node
        siblings_schema = vegx_schema_simple %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_parent() %>%  # Go up two levels to skip choice element
          xml_children() %>% 
          xml_attr("name")
        
        siblings_schema[which(is.na(siblings_schema))] = node_name # This is a bit hacky and will probably insert incorrectly when there are multiple unnamed elements at the same level, but there's no such case atm
        is_choice = FALSE    
      } else {
        siblings_schema = vegx_schema_simple %>% 
          xml_find_all(node_xpath) %>% 
          xml_parent() %>% 
          xml_children() %>% 
          xml_attr("name")
      }
      siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, node_name)]
      
      insert_position = which(siblings_ordered == node_name) - 1
      if(length(insert_position) == 0){insert_position = 0}
      
      # Insert node
      if(j < length(node_names[[i]])){
        if(!(node_name %in% siblings)){
          xml_add_child(parent, node_name, .where = insert_position)                   
        }
        parent = xml_child(parent, node_name)
      } else {
        # Get node schema definition
        node_def = vegx_schema_simple %>% 
          xml_find_all(node_xpath) %>% 
          xml_attrs() %>% 
          unlist()
        
        maxOccurs = ifelse("maxOccurs" %in% names(node_def), names(node_def)["maxOccurs"], "1")
        type = ifelse("type" %in% names(node_def), node_def["type"], "xsd:string")
        val = node_values[i]
        
        # Check if adding new node is allowed      
        if(!is.na(as.numeric(maxOccurs))){ # max_occ not unbounded
          if(length(siblings[siblings == node_name]) >= as.numeric(maxOccurs)){
            warning(paste0("Skipped node at '", paste(node_names[[i]][1:j], collapse = " > "), "': maxOccurs reached.")) 
            break
          }
        }
        
        # Do some format checks on node value
        if(type == "xsd:date"){
          val = ymd(val)
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_names[[i]][1:j], collapse = " > "), "': invalid date format."))
            break
          }
        } else if (type == "xsd:decimal"){
          val = as.numeric(val)
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_names[[i]][1:j], collapse = " > "), "': invalid numeric format."))
            break
          }
        } else if (type == "xsd:integer"){
          val = as.integer(val)
          if(is.na(val)){
            warning(paste0("Skipped node at '", paste(node_names[[i]][1:j], collapse = " > "), "': invalid integer format.")) 
            break
          }
        }
        xml_add_child(parent, node_name, as.character(val), .where = insert_position)  
      }
    }
  }
}