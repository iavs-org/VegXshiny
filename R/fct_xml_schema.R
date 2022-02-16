#' Load VegX schema
#' 
#' @description Loads a fresh copy of the source VegX XSD files
#' 
#' @return a list of xml2 documents
#' 
#' @importFrom xml2 read_xml
#' @importFrom here here
#' 
#' @noRd
load_schema = function(){
  schema_path = here::here("inst", "app", "www", "vegxschema")
  list(
    veg  = xml2::read_xml(paste0(schema_path, "/veg.xsd")),
    misc = xml2::read_xml(paste0(schema_path, "/veg-misc.xsd")),
    obs  = xml2::read_xml(paste0(schema_path, "/veg-plotobservation.xsd")),
    plot = xml2::read_xml(paste0(schema_path, "/veg-plot.xsd")),
    org  = xml2::read_xml(paste0(schema_path, "/veg-organism.xsd")),
    comm = xml2::read_xml(paste0(schema_path, "/veg-community.xsd"))
  )
}

# --------------------------------------------------------------------------------------- #
#' Simplify VegX XML schema and link across documents
#'
#' @description This is a recursive function to parse and simplify the VegX XSD schema. It is typically called on the root node (<xsd:element name="vegX"> in veg.xsd) and then works its way through the schema. In doing so, it does mainly two things: (1) remove 'clutter' such as attribute, annotation, sequence or complexType nodes and (2) link nodes across namespaces. 
#'
#' @param node An xml_node or xml_nodeset of length 1.
#' @param ns A character string specifying the VegX namespace of the current node (veg, misc, obs, plot, org, comm).
#' @param schema_files A named list with xsd definitions (see \link{load_schema}).
#' @param simplify Whether to remove container elements (complexType, sequence, simpleContent, extension) when linking the schema. Default True.
#'
#' @return This function is used for its side effects.
#'
#' @import dplyr
#' @import xml2
#' @import stringr
#'
#' @noRd
link_vegx_schema = function(node, ns, schema_files, simplify = T){
  # Pre-processing
  if(simplify){
    children = xml_children(node)
    
    # Include annotation elements as attribute
    annotation = children[xml_name(children) == "annotation"]
    if(length(annotation) > 0){
      xml_attr(node, "annotation") = xml_text(annotation)
    }
    
    # Include ID definition as attribute
    attributes = children[xml_name(children) == "attribute"]
    if(length(attributes) > 0){
      xml_attr(node, "id") = xml_attr(attributes, "use")[1]
    }
    
    # Remove annotation and attribute elements (but not attributes!)
    xml_remove(children[xml_name(children) %in% c("attribute", "annotation")]) 
    children = xml_children(node)
    
  } else {
    # Don't remove annotation and attribute nodes, leave as is
    children = xml_children(node)
    children = children[!xml_name(children) %in% c("attribute", "annotation")]
  }

  # Main logic
  if (length(children) == 0) {    # Base case: Leaf node
    if(xml_name(node) == "element" & xml_has_attr(node, "type")){
      type = xml_attr(node, "type")
      type_qualified = str_detect(type, pattern = ":")
      
      # If qualified, set new namespace
      if(type_qualified){    
        ns = str_split(type, ":", simplify = T)[1]
        type = str_split(type, ":", simplify = T)[2]
      }
      
      # If namespace belongs to VegX, graft type definition into current node
      if(ns %in% names(schema_files)){
        node_append = xml_find_all(schema_files[[ns]], str_glue("//*[@name='{type}']"))
        children_append = node_append %>% xml_children %>% xml_find_all("../*[not(self::xsd:annotation)]") # Avoid duplicate annotation type definition
        sapply(children_append, function(child){xml_add_child(node, child, .copy=T)})
        if(simplify & xml_has_attr(node_append, "id")){
          xml_set_attr(node, "id", xml_attr(node_append, "id"))   # If simplify = True and node_append has 'id' attribute, inherit 
        }
        link_vegx_schema(node, ns, schema_files, simplify)
      }
    } else {
      xml_remove(node) # This avoids issues with xsd:simpleType nodes with unnamed children
      return()
    }
  } else if(simplify & (xml_name(node) %in% c("complexType", "sequence", "simpleContent", "complexContent", "extension"))){    # Not a leaf, but a container
    parent = xml_parent(node)
    sapply(children, function(child){xml_add_child(parent, child, .copy=F)}) # attach all children to node's parent
    xml_remove(node)
    link_vegx_schema(parent, ns, schema_files, simplify) # Process simplified parent   
  } else {     # Not a leaf, not a container: Recursively process all children
    sapply(children, function(child){
      link_vegx_schema(child, ns, schema_files, simplify)
    })
  }
  return()
}

# --------------------------------------------------------------------------------------- #
#' Convert a VegX schema node and all its descendents to an R list object.
#'
#' @param node an xml_node or xml_nodeset of length 1
#' @param name_attr the XML attribute used for naming list elements
#' @param ns a character string specifying the VegX namespace of the current node (veg, misc, obs, plot, org, comm)
#' 
#' @description This is a recursive function to build an R list from a VegX document. 
#'
#' @return A list with a hierarchy corresponding to `node`
#'
#' @noRd
#' @import xml2
schema_to_list = function(node, name_attr, ns = character()){
  children = xml_children(node)
  if (length(children) == 0) {    # Base case: Leaf node
    return(NA_character_)
  } else {
    result = lapply(children, function(child){
      schema_to_list(child, name_attr, ns = ns)
    })
    children_names = ifelse(xml_has_attr(children, name_attr), # Condition
                 xml_attr(children, name_attr, ns = ns),       # True
                 paste0(xml_name(children, ns = ns)))          # False
    if (any(children_names != "")) {
      names(result) = children_names
    }
  }
  return(result)
}
