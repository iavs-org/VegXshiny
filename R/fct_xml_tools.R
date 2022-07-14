#' Load VegX schema
#' 
#' @description Loads a fresh copy of the source VegX XSD files
#' 
#' @return a list of xml2 documents
#' 
#' @importFrom xml2 read_xml
#' 
#' @noRd
load_schema = function(){
  list(
    veg  = xml2::read_xml(system.file("extdata", "vegxschema", "veg.xsd", package = "VegXshiny", mustWork = F)),
    misc = xml2::read_xml(system.file("extdata", "vegxschema", "veg-misc.xsd", package = "VegXshiny", mustWork = F)),
    obs  = xml2::read_xml(system.file("extdata", "vegxschema", "veg-plotobservation.xsd", package = "VegXshiny", mustWork = F)),
    plot = xml2::read_xml(system.file("extdata", "vegxschema", "veg-plot.xsd", package = "VegXshiny", mustWork = F)),
    org  = xml2::read_xml(system.file("extdata", "vegxschema", "veg-organism.xsd", package = "VegXshiny", mustWork = F)),
    comm = xml2::read_xml(system.file("extdata", "vegxschema", "veg-community.xsd", package = "VegXshiny", mustWork = F))
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
#' @param simplify Whether to remove container elements (complexType, sequence, simpleContent, extension, ...) when linking the schema. Default is TRUE.
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
      sapply(attributes, function(attribute){
        if(xml_has_attr(attribute, "use") && xml_attr(attribute, "use") == "required"){
          xml_attr(node, xml_attr(attribute, "name")) = xml_attr(attribute, "use")
        }
      })
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
  if(length(children) == 0) {    # Base case: Leaf node
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
        if(simplify){ # If node_append has 'id' or 'taxonName' attribute, inherit 
          if(xml_has_attr(node_append, "id")){xml_set_attr(node, "id", xml_attr(node_append, "id"))}
          if(xml_has_attr(node_append, "taxonName")){xml_set_attr(node, "taxonName", xml_attr(node_append, "taxonName"))}
        }
        link_vegx_schema(node, ns, schema_files, simplify)
      }
    }
    else {
      parent = xml_parent(node)
      if(xml_has_attr(node, "id")){xml_attr(parent, "id") = xml_attr(node, "id")}
      if(xml_has_attr(node, "taxonName")){xml_attr(parent, "taxonName") = xml_attr(node, "taxonName")}
      xml_remove(node) # This avoids issues with xsd:simpleType nodes with unnamed children
      return()
    }
  } else if(simplify & (xml_name(node) %in% c("complexType", "sequence", "simpleContent", "complexContent", "extension"))){    # Not a leaf, but a container
    parent = xml_parent(node)
    sapply(children, function(child){ # attach all children to node's parent
      xml_add_child(parent, child, .copy=F) 
    }) 
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

# --------------------------------------------------------------------------------------- #
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

# --------------------------------------------------------------------------------------- #
#' Create a VegX node
#' @description Creates and populates an instance of a VegX main element. Use `new_vegx_nodes()` when building many nodes at once.
#'
#' @param vegx_schema An `xml_document` of the vegx schema
#' @param node_paths A character vector representing the positions of the nodes to be created starting with the VegX main element. Levels are separated with a " > "
#' @param node_values A character vector containing the values of the nodes 
#' @param id The id given to the new node. If NULL (default), a new id will be created.``
#' @param log_path The path to the sessions temporary log file
#' 
#' @import dplyr
#' @import xml2
#' @importFrom stringr str_split
#' @importFrom lubridate ymd
#' 
#' @noRd
#' 
#' @return a 3-slot list containing the node and potential error and warning messages
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
    build_xml(target_root, node_names, unlist(node_values), vegx_schema)
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

# --------------------------------------------------------------------------------------- #
#' Create a VegX nodes
#' @description This function is a much faster alternative to repeated calls to `new_vegx_node()` when building many nodes.
#'
#' @param nodes_df A `data.frame` with node paths as column names and node values as values
#' @param vegx_schema An `xml_document` of the vegx schema
#' 
#' @noRd
#' 
#' @return a list of nodes
new_vegx_nodes = function(nodes_df, vegx_schema){
  tryCatch({
    #### Check node paths
    node_names = colnames(nodes_df) %>% stringr::str_split(" > ") 
    root_name = unique(sapply(node_names, "[[", 1))
    
    # Check node validity
    # 1. Are all paths within the same main element?
    if(length(root_name) != 1){stop(paste("Node paths points to multiple root elements:", root_name))}
    
    # 2. are nodepaths valid?
    node_xpaths = sapply(node_names, function(x){
      paste0(".//", paste0("*[@name='", x, "']", collapse = "/")) %>% 
        str_replace_all("\\*\\[@name='choice']", "xsd:choice")
    })
    schema_nodes = sapply(node_xpaths, xml_find_all, x = vegx_schema)
    
    if(any(lengths(schema_nodes) == 0)){        # if there are invalid node paths      
      if(all(lengths(schema_nodes) == 0)){
        stop("No valid node paths found.")
      }
      nodes_valid = which(lengths(schema_nodes) != 0) # Find valid nodes
      schema_nodes = schema_nodes[nodes_valid]        # Subset schema nodes
      node_names = node_names[nodes_valid]            # ... and node_names
      nodes_df = nodes_df[, nodes_valid, drop = F]    # ... and nodes_df to valid nodes
    }
    
    # 3. Do all mandatory nodes have values?
    node_xpaths_std = sapply(schema_nodes, xml_path)
    for(i in seq_along(schema_nodes)){
      node = schema_nodes[[i]]
      siblings_xpath_std = node %>% xml_siblings() %>% xml_path() 
      
      if(!xml2::xml_has_attr(node, "minOccurs") || xml2::xml_attr(node, "minOccurs") == 1){   # if node is mandatory --> set siblings (and all descendents) to "" where node value is missing
        node_values_missing = which(!sapply(nodes_df[,i], isTruthy))
        sibling_columns = setdiff(which(node_xpaths_std %in% siblings_xpath_std), i)
        if(length(node_values_missing) > 0 & length(sibling_columns) > 0){
          nodes_df[node_values_missing, sibling_columns] = ""
        }
      } else { # else -> check if all mandatory siblings are in nodes_df
        siblings_mandatory = sapply(siblings_xpath_std, function(xpath){
          sibling = xml_find_all(vegx_schema, xpath)
          !xml2::xml_has_attr(sibling, "minOccurs") || xml2::xml_attr(sibling, "minOccurs") == 1
        })
        siblings_mandatory = names(siblings_mandatory)[which(siblings_mandatory)]
        siblings_mandatory_present = sapply(siblings_mandatory, function(sibling){
          any(grepl(sibling, node_xpaths_std, fixed =T))
        })
        if(!all(siblings_mandatory_present)){
          nodes_df[,i] = ""
        }
      }
    }

    "/xsd:schema/xsd:element/xsd:element[1]/xsd:element/xsd:choice"
    "/xsd:schema/xsd:element/xsd:element[1]/xsd:element/xsd:choice/xsd:element[1]"
    # Find correct order of nodes according to schema
    leaf_nodes = xml_find_all(vegx_schema, paste0(".//*[@name='", root_name, "']")) %>% 
      xml_parent() %>% 
      xml_find_all(".//*[not(*)]")
    
    nodes_matched = match(unlist(sapply(schema_nodes, xml_path)), sapply(leaf_nodes, xml_path))   
    if(anyNA(nodes_matched)){warning(paste0("Invalid node path: ", node_xpaths[which(is.na(nodes_matched))]))}
    
    #### Prepare data
    # Reorder nodes_df according to schema
    node_names = node_names[order(nodes_matched)]
    nodes_df = nodes_df[, order(nodes_matched), drop = F]
    node_types = sapply(schema_nodes, function(node) xml_attr(node, "type"))[order(nodes_matched)]
    
    suppressWarnings({
      for(i in seq_along(node_types)){
        if(node_types[i] == "xsd:date"){
          nodes_df[,i] = as.character(lubridate::ymd(nodes_df[,i]))
        } else if(node_types[i] == "xsd:decimal"){
          nodes_df[,i] = as.numeric(nodes_df[,i])
        } else if(node_types[i] == "xsd:integer"){
          nodes_df[,i] = as.integer(nodes_df[,i])
        }
      }
    })
    
    # Build nodes
    nodes = lapply(1:nrow(nodes_df), function(i){
      root = xml_new_root(root_name)
      node_values = unlist(nodes_df[i,])
      
      for(j in 1:length(node_values)){
        if(is.na(node_values[j]) | node_values[j] == ""){next} # Skip node if empty
        if(length(node_names[[j]]) == 1){
          xml_text(root) = node_values[j] # no traversal needed, just set text value of root
        } else {
          parent = root
          for(k in 2:length(node_names[[j]])){
            node_name = node_names[[j]][k]
            if(node_name == "choice"){next}
            if(k < length(node_names[[j]])){
              if(! node_name %in% xml_name(xml_children(parent))){ # Only add new node if there is none with the same name
                xml_add_child(parent, node_name)
              } 
              parent = xml_child(parent, node_name)
            } else {
              xml_add_child(parent, node_name, as.character(node_values[j]))
            }
          }
        }
      } 
      # Check if root is empty
      if(xml_length(root) == 0 && xml_text(root) == ""){
        return(NULL)
      } else {
        id_key = paste0(root_name, "ID")
        if(id_key %in% names(id_factory)){
          id = id_factory[[id_key]]()
          xml_set_attr(root, "id", id)
        }
        return(list(node = root))
      }
    })
    return(nodes)
  })
}

# --------------------------------------------------------------------------------------- #
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
      new_action_log_record(log_path, "Merge info", paste0("Successfully merged mappings into ", root_name, " (id = ", root_id, ")."))
    }
  }
  
  return(list(warnings = length(conditions$warnings), errors = length(conditions$errors)))
}

# --------------------------------------------------------------------------------------- #
#' Build XML from vegx mappings
#' @description Utility function used by `*_vegx_node()` functions to build xml from node mappings. 
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

# --------------------------------------------------------------------------------------- #
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

# --------------------------------------------------------------------------------------- #
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