#' Convert a Turboveg xml document to a list of rectangular tables
#' 
#' @import xml2
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @return a list of dataframes
#'
#' @noRd

tv_to_df = function(tv_xml){
  # TODO 
  # The runtime complexity of xml2::xml_find_* (including xml_child, xml_parent, etc.) functions is horrible as xml documents get larger. 
  # A (still rather slow) workaround could be to add a releve_nr attribute to every relevant node and then extract attributes for the entire nodeset via xml_attrs, i.e.:
  # lapply(seq_along(plot_nodes), function(i){
  #   releve_nr = plot_nodes[[i]] %>% xml_attr("releve_nr")
  #   xml_find_all(plot_nodes[[i]], "./species_data/species/standard_record") %>% xml_set_attr("releve_nr", releve_nr) 
  #   plot_nodes[[i]] %>% xml_child("species_data") 
  # }) 
  # species_df = xml_find_all(tv_xml, ".///species_data/species/standard_record") %>% xml_attrs()
  # ----------------------------------------------------------- #
  # Plot data ####
  plot_nodes = tv_xml %>% xml_child("Plots") %>% xml_children() 

  plot_dfs = lapply(seq_along(plot_nodes), function(i){
    releve_nr = plot_nodes[[i]] %>% xml_attr("releve_nr")

    # Header data - Standard records
    standard_header_df = plot_nodes[[i]] %>% 
      xml_child("header_data") %>% 
      xml_child("standard_record") %>% 
      xml_attrs() %>% 
      as.data.frame.list() %>% 
      mutate(releve_nr = releve_nr) %>% 
      relocate(releve_nr)
    
    # Header data - Undefined records
    udf_header_df = plot_nodes[[i]] %>% 
      xml_find_all("./header_data/udf_record") %>% 
      xml_attrs() %>% 
      bind_rows() %>% 
      dplyr::select(name, value) %>% 
      tidyr::pivot_wider(names_from = "name", values_from = value) %>% 
      mutate(releve_nr = releve_nr) %>% 
      relocate(releve_nr)
    
    # Species data 
    species_nodes = plot_nodes[[i]] %>% 
      xml_find_all("./species_data/species")
    
    species_df = lapply(seq_along(species_nodes), function(j){
      # Standard data
      species_record = species_nodes[[j]] %>% 
        xml_child("standard_record") %>% 
        xml_attrs() %>% 
        as.data.frame.list() %>% 
        mutate(releve_nr = releve_nr)

      if(length(xml_child(species_nodes[[j]], "udf_record")) != 0){
        udf_species_record = species_nodes[[j]] %>%
          xml_child("udf_record") %>%
          xml_attrs() %>%
          as.data.frame.list() %>%
          dplyr::select(name, value) %>%
          tidyr::pivot_wider(names_from = name, values_from = value)
        species_record = cbind(species_record, udf_species_record)
      }
      
      return(species_record)
    }) %>% bind_rows() %>% 
      relocate(releve_nr)

    return(list(standard_header = standard_header_df, udf_header = udf_header_df, species = species_df))
  })
  
  # ----------------------------------------------------------- #
  # Lookup tables ####
  lookup_names = tv_xml %>% xml_child("Lookup_tables") %>% xml_children() %>% xml_name()
  lookup_dfs = sapply(lookup_names, simplify = FALSE, USE.NAMES = TRUE, FUN = function(name){
    # Select all leaf nodes in current lookup category
    lookup_parent_nodes = tv_xml %>% 
      xml_child("Lookup_tables") %>% 
      xml_child(name) %>% 
      xml_find_all(".//*[not(*)]") %>% 
      xml_parent()
    
    # Extract data
    lookup_df = lapply(seq_along(lookup_parent_nodes), function(i){
      lookup = list()
      nodes = lookup_parent_nodes[[i]] %>% xml_children()
      for(j in seq_along(nodes)){
        node_name = xml_name(nodes[[j]])
        node_text = xml_text(nodes[[j]])
        if(node_text != ""){                 # Special element with text node
          lookup[[node_name]] = node_text  
        } else {                             # Standard turboveg record
          record = nodes[[j]] %>% xml_attrs() %>% as.data.frame.list()
          lookup[["records"]] = append(lookup[["records"]], list(record))
        }  
      }
      lookup$records = bind_rows(lookup$records)
      return(lookup)
    })
    
    return(lookup_df)
  })
  
  # ----------------------------------------------------------- #
  # Templates? ####
  
  # Prepare result
  standard_header_df = lapply(plot_dfs, "[[", "standard_header") %>% bind_rows()
  udf_header_df = lapply(plot_dfs, "[[", "udf_header") %>% bind_rows()
  species_df = lapply(plot_dfs, "[[", "species") %>% bind_rows()
  
  tv_import = list(std_header = standard_header_df,
                   udf_header = udf_header_df,
                   species = species_df,
                   lookup = lookup_dfs)
  return(tv_import)
}

