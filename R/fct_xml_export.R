#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element
#'
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values The values of the nodes 
#'
#' @return xml_document
mappings_to_vegx = function(node_paths, node_values){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  if(length(root_name) != 1){
    stop("Node paths do not share the same root")
  }
  
  # Build XML
  tmp_doc = xml_new_root(root_name)
  for(i in 1:length(node_names)){
    parent = tmp_doc
    for(j in 2:length(node_names[[i]])){ # ignore root node
      name = node_names[[i]][j]
      siblings = xml_children(parent)
      if(j == length(node_names[[i]])){
        xml_add_child(parent, name, node_values[i])
      } else {
        if (!name %in% xml_name(siblings)){
          xml_add_child(parent, name)
        }
        parent = xml_child(parent, name)
      }
    }
  }
  
  # set IDs
  
  root = xml_find_all(tmp_doc, ".")
  return(root)
}

vegx_add_nodes = function(){
  
}

vegx_update_nodes = function(){
  
}
