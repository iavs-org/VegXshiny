#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element
#'
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values The values of the nodes 
#' 
#' @import dplyr
#' @import xml2
#' @import stringr
#' 
#' @noRd
#' 
#' @return xml_document
new_vegx_node = function(node_paths, node_values, create_id = T){
  # Prepare data
  node_names = node_paths %>% str_split(" > ")
  root_name = unique(sapply(node_names, function(names){names[1]}))
  if(length(root_name) != 1){
    stop("Node paths do not share the same root")
  }
  
  # Build XML
  # TODO respect sequence order defined in schema
  tmp_doc = xml_new_root(root_name)
  for(i in 1:length(node_names)){
    parent = tmp_doc
    if(is.na(node_values[i]) | node_values[i] == ""){next}
    for(j in 2:length(node_names[[i]])){ # ignore root node
      xpath = paste0("..//*[@name='", paste(node_names[[i]][1:j], collapse = "']//*[@name='"), "']") 
      name = node_names[[i]][j]
      if(name == "choice"){next}
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
  
  # set ID for root node
  root_node = xml_find_all(tmp_doc, ".")
  root_definition = xml_find_all(vegx_schema_simple, paste0("//*[@name='", root_name, "']"))
  if(create_id & xml_has_attr(root_definition, "id")){
    generator_name = id_lookup[paste0(root_name,"ID")]
    new_id = id_factory[[generator_name]]()
    xml_set_attr(root_node, "id", new_id)
  }
  
  if(length(xml_children(root_node)) == 0){
    return(NA)
  } else {
    return(root_node)
  }
}

# library(dplyr)
# library(stringr)
# library(xml2)
# node_paths =  c("project > personnel > partyID", "project > personnel > role", "project > abstract")
# node_values = c("1", "helper", "Exciting study")

