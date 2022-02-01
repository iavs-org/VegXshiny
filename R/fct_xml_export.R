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
  tmp_doc = xml_new_root(root_name)
  
  # Build XML
  for(i in 1:length(node_names)){
    if(is.na(node_values[i]) | node_values[i] == ""){next} # Skip empty mappings
    
    parent = tmp_doc
    for(j in 2:length(node_names[[i]])){ # ignore root node
      name = node_names[[i]][j]
      if(name == "choice"){next}
      # Determine node order
      siblings = xml_children(parent) %>% xml_name()
      siblings_schema =  xml_find_all(vegx_schema_simple, paste0("..//*[@name='", paste(node_names[[i]][1:j-1], collapse = "']//*[@name='"), "']")) %>% 
        xml_children() %>% 
        xml_attr("name")
      siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, name)]
      
      # Determine insert position
      insert_position = which(siblings_ordered == name) - 1
      if(length(insert_position) == 0){insert_position = 0} # Insert at beginning, if no siblings present
      
      # Insert node
      if(j == length(node_names[[i]])){            
        xml_add_child(parent, name, node_values[i], .where = insert_position)   # Insert leaf nodes with value
      } else {
        xml_add_child(parent, name,  .where = insert_position)                  # Otherwise, just insert in right position
        parent = xml_child(parent, name)
      }
    }
  }
  
  # set ID for root node
  root_node = xml_find_all(tmp_doc, ".")
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
  # TODO respect sequence order defined in schema
  tmp_root = xml_find_all(vegx_doc, paste0("//", root_name, "[@id='", target_node_id, "']"))
  
  for(i in 1:length(node_names)){
    if(is.na(node_values[i]) | node_values[i] == ""){next} # Skip empty mappings
    
    parent = tmp_root
    for(j in 2:length(node_names[[i]])){ # ignore root node
      name = node_names[[i]][j]
      if(name == "choice"){next}
      # Determine node order
      siblings = xml_children(parent) %>% xml_name()
      siblings_schema =  xml_find_all(vegx_schema_simple, paste0("..//*[@name='", paste(node_names[[i]][1:j-1], collapse = "']//*[@name='"), "']")) %>% 
        xml_children() %>% 
        xml_attr("name")
      siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, name)]
      
      # Determine insert position
      insert_position = which(siblings_ordered == name) - 1
      if(length(insert_position) == 0){insert_position = 0} # Insert at beginning, if no siblings present
      
      # Insert node
      if(j == length(node_names[[i]])){            
        xml_add_child(parent, name, node_values[i], .where = insert_position)   # Insert leaf nodes with value
      } else {
        xml_add_child(parent, name,  .where = insert_position)                  # Otherwise, just insert in right position
        parent = xml_child(parent, name)
      }
    }
  }

  return(NULL)
}