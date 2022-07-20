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
#' @description This is a recursive function to parse and simplify the VegX XSD schema. It is typically called on the root node 
#' (<xsd:element name="vegX"> in veg.xsd) and then works its way through the schema. In doing so, it does mainly two things: 
#' (1) remove 'clutter' such as attribute, annotation, sequence or complexType nodes and 
#' (2) link nodes across namespaces. 
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
        children_append = node_append %>% xml_children
        if(xml_has_attr(node, "annotation") || length(xml_child(node, "annotation")) != 0){
          children_append = children_append %>% xml_find_all("../*[not(self::xsd:annotation)]") # Avoid duplicate annotation type definition  
        }
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
#' Create a VegX nodes
#' @description Build VegX nodes from a dataframe
#'
#' @param nodes_df A `data.frame` with node paths as column names and node values as values
#' @param vegx_schema An `xml_document` of the vegx schema
#' 
#' @noRd
#' 
#' @return a list of nodes
new_vegx_nodes = function(nodes_df, vegx_schema, id_factory){
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
    nodes = lapply(1:nrow(nodes_df), function(i, id_factory){
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
        return(root)
      }
    }, id_factory)
    return(nodes)
  })
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
templates_to_nodes = function(templates_selected, vegx_schema, id_factory){     
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
      main_element = unique(nodes_split[[j]]$main_element)
      node_links = str_which(nodes_split[[j]]$node_value, "^\\{[0-9]+\\}$") %>% as.numeric() # row indices of links
      if(length(node_links) > 0){
        for(k in node_links){
          # replace link with actual id
          linked_node_index = nodes_split[[j]]$node_value[k] %>% stringr::str_extract("[0-9]+") %>% as.numeric()
          linked_node_id = new_nodes[[linked_node_index]] %>% xml2::xml_attr("id")
          nodes_split[[j]]$node_value[k] = linked_node_id 
        }
      }
      node_df = nodes_split[[j]] %>% 
        tidyr::pivot_wider(names_from = node_path, values_from = node_value) %>% 
        dplyr::select(-template_id, -node_id, -main_element)
      
      new_node = new_vegx_nodes(node_df, vegx_schema, id_factory)
      new_nodes = append(new_nodes, new_node)
      node_list[[main_element]] = append(node_list[[main_element]], new_node)
    }
  }
  return(node_list)
}

check_document_links = function(vegx_doc) {
  # Helper functions
  
  # missingTargets: Get a vector of id's where references
  # point to. After that, check if the respective target elements exist.
  #
  # linkName      One unambiguous string such as "<methodID> (with brackets!)"
  #               defining the link
  # targetName    One unambiguous string such as "method id" defining the
  #               target
  missingTargets <- function(linkName, targetName) {
    # Find links
    links <- vegx[grep(linkName, vegx)]
    patternID <- paste(".*", linkName, "|", "<.*", sep = "")
    ids <- sort(unique(gsub(patternID, "", links)))
    
    # Find targets
    occurrencesTarget <- grep(targetName, vegx)
    patternTar <- paste(".*", "id=\"", "|", "\".*", sep = "")
    targets <- sub(" .*$", "", gsub(patternTar, "", vegx[occurrencesTarget]))
    
    # Find missing targets
    nonexisting <- ids[which(!ids %in% targets)]
    
    # Rows with missing targets
    linkidx <- which(vegx %in% links)
    missingString <- paste(linkName, nonexisting, "</", sep = "")
    problemRow <- linkidx[grep(missingString, vegx[linkidx])]
    if (sum(!ids %in% targets) > 0) {
      linkName = stringr::str_replace_all(linkName, c("<" = "", ">" = ""))
      cat("Reference(s) to ", linkName, " point to non-existing nodes. (lines: ", paste0(problemRow, collapse = ", "), ")\n", sep = "")
    }
  }  
  
  # orphanTargets: Check if there are orphan elements (i.e. elements
  # that should be target of some link, which are not targeted)
  #
  # targetName  One unambiguous string defining the target lines such as "method id"
  # linkNames   Vector with one or more unambiguous strings such as 
  #             c("<methodID>", "<qualityAssessmentMethodID>") defining the
  #             linking lines
  
  orphanTargets <- function(targetName, linkNames) {
    
    # Find target
    targetLines <- vegx[grep(targetName, vegx)]
    patternTar <- paste(".*", "id=\"", "|", "\".*", sep = "")
    targets <- sub(" .*$", "", gsub(patternTar, "", targetLines))
    
    # Find links
    linkNamesString <- paste(linkNames, collapse = "|")
    linkLines <- vegx[grep(linkNamesString, vegx)] # Lines with occurrences of link strings  
    patternLink <- paste(paste(".*", linkNames, "|", "<.*", sep = ""), collapse = "|") # pattern used for isolating actual id's
    ids <- sort(unique(gsub(patternLink, "", linkLines)))
    
    # Find orphans
    nonexisting <- targets[which(!targets %in% ids)]
    
    # Rows with orphans
    targetIdx <- which(vegx %in% targetLines)
    missingString <- paste(targetName, "=\"", nonexisting, "\">", sep = "")
    problemRow <- grep(paste(missingString, collapse="|"), vegx)
    
    if (sum(!targets %in% ids) > 0) {
      cat(stringr::str_to_sentence(targetName), " node(s) with id(s) ", paste0(nonexisting, collapse = ", "), 
          " have no incoming references. (lines: ", paste0(problemRow, collapse = ", "), ")\n", sep = "")
    }  
  }
  
  vegx = vegx_doc %>% as.character() %>% stringr::str_split("\n", simplify = T)
  
  missing_targets = capture.output({
    # First check if all target ID's exist  
    missingTargets("<methodID>",                      "method id")
    missingTargets("<qualityAssessmentMethodID>",     "method id")
    missingTargets("<attributeID>",                   "attribute id")
    missingTargets("<qualitativeAttributeID>",        "attribute id")
    missingTargets("<protocolID>",                    "protocol id")
    missingTargets("<citationID>",                    "literatureCitation id")
    missingTargets("<accordingToCitationID>",         "literatureCitation id")
    missingTargets("<interpretationCitationID>",      "literatureCitation id")
    missingTargets("<documentCitationID>",            "literatureCitation id")
    missingTargets("<organismNameID>",                "organismName id")
    missingTargets("<originalOrganismNameID>",        "organismName id")
    missingTargets("<preferredOrganismNameID>",       "organismName id")
    missingTargets("<organismIdentityID>",            "organismIdentity id")
    missingTargets("<partyID>",                       "party id")
    missingTargets("<determinationPartyID>",          "party id")
    missingTargets("<originalIdentificationPartyID>", "party id")
    missingTargets("<conceptAssertionPartyID>",       "party id")
    missingTargets("<interpretationPartyID>",         "party id")
    missingTargets("<placementPartyID>",              "party id")
    missingTargets("<locationPartyID>",               "party id")
    missingTargets("<elevationPartyID>",              "party id")
    missingTargets("<depthPartyID>",                  "party id")
    missingTargets("<observationPartyID>",            "party id")
    missingTargets("<taxonConceptID>",                "taxonConcept id")
    missingTargets("<communityConceptID>",            "communityConcept id")
    missingTargets("<projectID>",                     "project id")
    missingTargets("<relatedProjectID>",              "project id")
    missingTargets("<plotID>",                        "plot id")
    missingTargets("<relatedPlotID>",                 "plot id")
    missingTargets("<relatedItemID>",                 "individualOrganism id")
    missingTargets("<stratumObservationID>",          "stratumObservation id")
    missingTargets("<communityObservationID>",        "communityObservation id")
    missingTargets("<siteObservationID>",             "siteObservation id")
    missingTargets("<plotObservationID>",             "plotObservation id")
    missingTargets("<previousPlotObservationID>",     "plotObservation id")
    missingTargets("<observationGroupingID>",         "observationGrouping id")
    missingTargets("<stratumID>",                     "stratum id")
    missingTargets("<surfaceTypeID>",                 "surfaceType id")
  })
  
  # Check for orphans
  missing_parents = capture.output({
    orphanTargets("party id",                c("<partyID>", 
                                               "<determinationPartyID>",
                                               "<originalIdentificationPartyID>",
                                               "<conceptAssertionPartyID>",
                                               "<interpretationPartyID>",
                                               "<placementPartyID>",
                                               "<locationPartyID>",
                                               "<elevationPartyID>",
                                               "<depthPartyID>",
                                               "<observationPartyID>"))
    orphanTargets("literatureCitation id",   c("<citationID>",
                                               "<accordingToCitationID>",
                                               "<interpretationCitationID>",
                                               "<documentCitationID>"))
    orphanTargets("method id",               c("<methodID>",
                                               "<qualityAssessmentMethodID>"))
    orphanTargets("protocol id",               "<protocolID>")
    orphanTargets("organismIdentity id",       "<organismIdentityID>")     
    orphanTargets("taxonConcept id",           "<taxonConceptID>")
    orphanTargets("communityConcept id",       "<communityConceptID>")
    orphanTargets("project id",              c("<projectID>",
                                               "<relatedProjectID>"))
    orphanTargets("plot id",                 c("<plotID>",
                                               "<relatedPlotID>"))
    orphanTargets("communityObservation id",   "<communityObservationID>")
    orphanTargets("siteObservation id",        "<siteObservationID>")
    orphanTargets("plotObservation id",      c("<plotObservationID>",
                                               "<previousPlotObservationID>"))
    orphanTargets("observationGrouping id",    "<observationGroupingID>") 
    orphanTargets("surfaceType id",            "<surfaceTypeID>")
    orphanTargets("individualOrganism id",   c("<relatedItemID>", 
                                               "<individualOrganismID>"))
    orphanTargets("organismName id",         c("<organismNameID>",
                                               "<originalOrganismNameID>",
                                               "<preferredOrganismNameID>"))
  })
  
  return(c(missing_targets, missing_parents))
} 
