#' Simplify VegX XML schema and link across documents
#'
#' @param node an xml_node or xml_nodeset of length 1
#' @param ns a character string specifying the VegX namespace of the current node (veg, misc, obs, plot, org, comm)
#' 
#' @description This is a recursive function to parse and simplify the VegX XSD schema. It is typically called on the root node (<xsd:element name="vegX"> in veg.xsd) and then works its way through the schema. In doing so, it does mainly two things: (1) remove clutter such as attribute, annotation, sequence or complexType nodes and (2) link nodes across documents.
#'
#' @return This function does not return anything.
#'
#' @noRd
#' @import xml2
#' @importFrom stringr str_glue str_detect str_split

simplify_vegx_node = function(node, ns, namespaces){
  children = xml_children(node)
  
  # Include annotation elements as attribute
  annotation = children[xml_name(children) == "annotation"]
  if(length(annotation) == 1){
    xml_attr(node, "annotation") = xml_text(annotation)
  }
  
  # Then remove annotation and attribute elements (but not attributes of the root)
  xml_remove(children[xml_name(children) %in% c("attribute", "annotation")], free = T)
  children = xml_children(node)
  
  # Process node based on its 
  if (length(children) == 0) {    # Base case: Leaf node
    if(xml_name(node) == "element" & xml_has_attr(node, "type")){  # If leaf node is a VegX element, graft type definition into element
      type = xml_attr(node, "type")
      type_qualified = str_detect(type, pattern = ":")
      if(type_qualified){    # Find type definition in another namespace, ignore XSD namespace
        ns = str_split(type, ":", simplify = T)[1]
        type = str_split(type, ":", simplify = T)[2]
        if(ns %in% names(namespaces)){
          node_append = xml_find_all(namespaces[[ns]], str_glue("//*[@name='{type}']"))
          children_append = xml_children(node_append)
          sapply(children_append, function(child){xml_add_child(node, child, .copy=T)})
          simplify_vegx_node(node, ns, namespaces)
        }
      } else { # Find type definition in current namespace
        node_append = xml_find_all(namespaces[[ns]], str_glue("//*[@name='{type}']"))
        children_append = xml_children(node_append)
        sapply(children_append, function(child){xml_add_child(node, child, .copy=T)})
        simplify_vegx_node(node, ns, namespaces)
      }
    } else {
      return()
    }
  } else if(xml_name(node) %in% c("complexType", "sequence")){    # Skip/Remove containers (sequences, complexTypes)
    parent = xml_parent(node)
    sapply(children, function(child){xml_add_child(parent, child, .copy=F)})
    xml_remove(node)
    simplify_vegx_node(parent, ns, namespaces) # Process simplified parent 
  } else {     # Process all children
    sapply(children, function(child){
      simplify_vegx_node(child, ns, namespaces)
    })
  }
}

#' Convert an VegX XML document to an R list
#'
#' @param node an xml_node or xml_nodeset of length 1
#' @param name_attr the XML attribute used for naming list elements
#' @param ns a character string specifying the VegX namespace of the current node (veg, misc, obs, plot, org, comm)
#' 
#' @description This is a recursive function to build an R list from a VegX XML document. 
#'
#' @return A list with a structure corresponding to node
#'
#' @noRd
#' @import xml2

vegx_to_list = function(node, name_attr, ns = character()){
  children = xml_children(node)
  if (length(children) == 0) {    # Base case: Leaf node
    return(NA_character_)
  } else {
    out = lapply(children, function(child){
      vegx_to_list(child, name_attr, ns = ns)
    })
    nms = ifelse(xml_has_attr(children, name_attr),            # Condition
                 xml_attr(children, name_attr, ns = ns),       # True
                 paste0(xml_name(children, ns = ns))) # False
    if (any(nms != "")) {
      names(out) = nms
    }
  }
  return(out)
}
