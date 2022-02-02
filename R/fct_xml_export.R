#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element
#'
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values The values of the nodes 
#' @param id The id given to the new node. If NULL (default), a new id will be created.
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' 
#' @noRd
#' 
#' @return xml_document
new_vegx_node = function(node_paths, node_values, id = NULL){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  if(length(root_name) != 1){
    shiny::showNotification("Node paths do not share the same root", type = "error")
    return()
  }
  
  # Build XML
  tmp_root = xml_new_root(root_name)
  build_xml(tmp_root, node_names, node_values)
  
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
merge_into_vegx_node = function(target_node_id, node_paths, node_values){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  if(length(root_name) != 1){
    stop("Node paths do not share the same root")
  }
  
  # Build XML
  tmp_root = xml_find_all(vegx_doc, paste0("//", root_name, "[@id='", target_node_id, "']"))
  build_xml(tmp_root, node_names, node_values)
  
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
#' @import lubridate
#' 
#' @noRd
#' 
#' @return This function is used for its side effects.
build_xml = function(root, node_names, node_values){
  for(i in 1:length(node_names)){
    if(is.na(node_values[i]) | node_values[i] == ""){next} # Skip empty mappings

    parent = root
    for(j in 2:length(node_names[[i]])){ # ignore root node
      name = node_names[[i]][j]
      if(name == "choice"){
        next
      }
      
      # Get node attributes
      siblings =  xml_children(parent) %>% xml_name()
      xpath_node = paste0("..//*[@name='", paste(node_names[[i]][1:j], collapse = "']//*[@name='"), "']") %>% 
        str_replace_all("\\*\\[@name='choice']", "xsd:choice")
      node_def = vegx_schema_simple %>% 
        xml_find_all(xpath_node) %>% 
        xml_attrs() %>% 
        unlist()

      # Check if adding new node is allowed      
      maxOccurs = ifelse("maxOccurs" %in% names(node_def), names(node_def)["maxOccurs"], "1")
      if(!is.na(as.numeric(maxOccurs))){ # max_occ not unbounded
        if(length(siblings[siblings == name]) >= as.numeric(maxOccurs)){
          showNotification("Could not add node") # elaborate
          break
        }
      }
      
      # Determine insertion position
      siblings_schema = vegx_schema_simple %>% 
        xml_find_all(xpath_node) %>% 
        xml_parent() %>% 
        xml_children() %>% 
        xml_attr("name")
      
      siblings_schema[is.na(siblings_schema)] = name # This is a bit hacky and will probably insert incorrectly when there are multiple unnamed elements at the same level
      siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, name)]

      insert_position = which(siblings_ordered == name) - 1
      if(length(insert_position) == 0){insert_position = 0}                     # If no siblings present, insert in the beginning
      
      # Insert node
      if(j < length(node_names[[i]])){
        xml_add_child(parent, name, .where = insert_position)                   
        parent = xml_child(parent, name)
      } else {
        # Do some format checks on node value
        val = node_values[i]
        type = ifelse("type" %in% names(node_def), node_def["type"], "xsd:string")
        
        if(type == "xsd:date"){
          val = lubridate::ymd(val)
          if(is.na(val)){
            showNotification("Invalid date") # elaborate
            break
          }
        } else if (type == "xsd:decimal"){
          val = as.numeric(val)
          if(is.na(val)){
            showNotification("Invalid decimal") # elaborate
            break
          }
        } else if (type == "xsd:integer"){
          val = as.integer(val)
          if(is.na(val)){
            showNotification("Invalid integer", type = "error") # elaborate
            break
          }
        }

        xml_add_child(parent, name, as.character(val), .where = insert_position)  
      }
    }
  }
}