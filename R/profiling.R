# library(profvis)
# library(xml2)
# library(dplyr)
# library(stringr)
# 
# source("./R/fct_xml_schema.R")
# source("./R/fct_xml_export.R")
# 
# schema_files = load_schema()
# vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
# link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)
# 
# node_paths = list(c("party","choice", "individualName"), c("party", "address"), c("party","phone"), c("party", "electronicMailAddress"), c("party", "onlineURL"))
# node_values = c("A", "B", "C", "D", "E")
# vegx_schema = vegx_schema %>% xml_find_all("*[@name='parties']")

# # 1. new_vegx_node
# 
# profvis({
#   for(run in 1:1000){
#     root = xml_new_root("party")
#     # Main loop
#     for(i in 1:length(node_paths)){
#       if(is.na(node_values[i]) | node_values[i] == ""){
#         warning(paste0("Skipped node at '", paste(node_paths[[i]][1], collapse = " > "), "' and descendents: no node value found."))
#         break
#       }
# 
#       parent = root
#       is_choice = FALSE
#       if(length(node_paths[[i]]) == 1){ # no traversal needed, just set text value of root
#         node_xpath = paste0(".//*[@name='", node_paths[[i]], "']")
#         if(length(xml_find_all(vegx_schema, node_xpath)) == 0){ # Check if node name is valid
#           warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: invalid node name."))
#           break
#         }
#         xml_text(root) = node_values[i]
#       } else { # traverse node path and create new nodes
#         for(j in 2:length(node_paths[[i]])){
#           node_name = node_paths[[i]][j]
# 
#           # Skip choices
#           if(node_name == "choice"){
#             is_choice = TRUE # Next element is a choice element, leave parent as is
#             next
#           }
# 
#           node_xpath = paste0(".//xsd:element[@name='", paste(node_paths[[i]][1:j], collapse = "']//*[@name='"), "']") %>%
#             str_replace_all("\\*\\[@name='choice']", "xsd:choice")
#           schema_node = xml_find_all(vegx_schema, node_xpath)
# 
#           # Check if node name is valid
#           if(length(schema_node) == 0){
#             warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: invalid node name."))
#             break
#           }
# 
#           # Determine insertion position
#           siblings = xml_children(parent) %>% xml_name()
#           if(is_choice){ # If node is child of a choice element
#             choices = schema_node %>%
#               xml_parent() %>%
#               xml_children %>%
#               xml_attr("name")
# 
#             # Check other choice is present
#             if(length(intersect(siblings, choices[choices != node_name])) != 0){
#               warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "' and descendents: only one node allowed in this position."))
#               break
#             }
#             # If not, find siblings of choice element instead of siblings of the node
#             siblings_schema = schema_node %>%
#               xml_parent() %>%
#               xml_parent() %>%  # Go up two levels to skip choice element
#               xml_children() %>%
#               xml_attr("name")
# 
#             siblings_schema[which(is.na(siblings_schema))] = node_name # This is a bit hacky and will probably insert incorrectly when there are multiple unnamed elements at the same level, but there's no such case atm
#             is_choice = FALSE
#           } else {
#             siblings_schema_nodes = schema_node %>%
#               xml_parent() %>%
#               xml_children()
# 
#             if("choice" %in% xml_name(siblings_schema_nodes)){
#               siblings_schema = siblings_schema_nodes %>% xml_attr("name")
#               choices = siblings_schema_nodes[which(xml_name(siblings_schema_nodes) == "choice")] %>% xml_children() %>% xml_attr("name")
#               siblings_schema = append(siblings_schema, choices, after = which(is.na(siblings_schema)))
#               siblings_schema = siblings_schema[!is.na(siblings_schema)]
#             } else {
#               siblings_schema = siblings_schema_nodes %>% xml_attr("name")
#             }
#           }
#           siblings_ordered = siblings_schema[siblings_schema %in% c(siblings, node_name)]
# 
#           insert_position = which(siblings_ordered == node_name) - 1
#           if(length(insert_position) == 0){
#             insert_position = 0
#           }
#           if(length(insert_position) > 1){
#             warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': Multiple matches in VegX schema."))
#           }
# 
#           # Insert node
#           if(j < length(node_paths[[i]])){
#             if(!(node_name %in% siblings)){
#               xml_add_child(parent, node_name, .where = insert_position)
#             }
#             parent = xml_child(parent, node_name)
#           } else {
#             # Get node schema definition
#             node_def = schema_node %>%
#               xml_attrs() %>%
#               unlist()
# 
#             node_def_names = names(node_def)
#             maxOccurs = 1
#             if("maxOccurs" %in% node_def_names){maxOccurs = node_def_names["maxOccurs"]}
#             type = "xsd:string"
#             if("type" %in% node_def_names){type = node_def["type"]}
#             val = node_values[i]
# 
#             # Check if adding new node is allowed
#             if(!is.na(as.numeric(maxOccurs))){ # max_occ not unbounded
#               if(length(siblings[siblings == node_name]) >= as.numeric(maxOccurs)){
#                 warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': maxOccurs reached."))
#                 break
#               }
#             }
# 
#             # Do some format checks on node value
#             if(type == "xsd:date"){
#               val = suppressWarnings(ymd(val))
#               if(is.na(val)){
#                 warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid date format."))
#                 break
#               }
#             } else if (type == "xsd:decimal"){
#               val = suppressWarnings(as.numeric(val))
#               if(is.na(val)){
#                 warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid decimal format."))
#                 break
#               }
#             } else if (type == "xsd:integer"){
#               val = suppressWarnings(as.integer(val))
#               if(is.na(val)){
#                 warning(paste0("Skipped node at '", paste(node_paths[[i]][1:j], collapse = " > "), "': invalid integer format."))
#                 break
#               }
#             }
#             xml_add_child(parent, node_name, as.character(val), .where = insert_position)
#           }
#         }
#       }
#     }
# 
#     # Cleanup: remove remains of failed insertions
#     leaf_nodes = xml_find_all(root, ".//*[not(*)]")
#     empty_leaf_nodes = leaf_nodes[xml_text(leaf_nodes) == ""]
# 
#     if(length(empty_leaf_nodes) != 0){
#       for(empty_leaf in empty_leaf_nodes){
#         parent = xml_parent(empty_leaf)
#         xml_remove(empty_leaf)
#         while(length(xml_children(parent)) == 0 & length(xml_parents(parent)) != 0){
#           parent_new = xml_parent(parent)
#           xml_remove(parent)
#           parent = parent_new
#         }
#       }
#     }
#   }
# })
# 
# # ----------------------------------------------- #
# # 2. finding node definition
# 
# root_name = "aggregateOrganismObservations"
# system.time({
#   replicate(5000,{
#     root_definition = xml_find_first(vegx_schema, paste0(".//*[@name='", root_name, "']"))
#   })
# })
# 
# vegx_schema_parties = vegx_schema %>% xml_find_all("*[@name='parties']")
# system.time({
#   replicate(5000,{
#     root_definition = xml_find_first(vegx_schema_aggOrgObs, paste0(".//*[@name='", root_name, "']"))
#   })
# })
# 
# # ----------------------------------------------- #
# # 3. add nodes to an xml_document
# main_doc = xml_new_root("root")
# nodes = lapply(1:1000, function(i){
#   xml_new_root("A") %>% xml_add_child("B")})
# system.time({
#   for(node in nodes){
#     xml_add_child(main_doc, node)
#   }
# })
# 
# main_doc2 = xml_new_root("root")
# nodes = lapply(1:1000, function(i){
#   xml_new_root("A") %>% xml_add_child("B")})
# system.time({
#   xml_add_child(main_doc2, nodes[[1]])
#   target_node = xml_child(main_doc2, 1)
#   for(i in 2:length(nodes)){
#     xml_add_sibling(target_node, nodes[[i]])
#   }
# })
# 
# nodes_df = data.frame("party > choice > individualName" = sample(LETTERS, 5000, replace = T),
#                       "party > address" = sample(LETTERS, 5000, replace = T),
#                       "party > phone" = sample(LETTERS, 5000, replace = T),
#                       "party > electronicMailAddress" = sample(LETTERS, 5000, replace = T),
#                       "party > onlineURL" = sample(LETTERS, 5000, replace = T),
#                       check.names = F)
# 
# system.time({new_nodes = new_vegx_nodes(nodes_df, vegx_schema)})
# 
# system.time({
#   new_nodes2 = lapply(1:nrow(nodes_df), function(i){
#     new_vegx_node(colnames(nodes_df), nodes_df[i,], id = NULL, log_path, vegx_schema, write_log = F)
#   })
# })