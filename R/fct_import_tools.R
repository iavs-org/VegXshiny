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
  # ----------------------------------------------------------- #
  # Plot data ####
  plot_nodes = tv_xml %>% xml_child("Plots") %>% xml_children()
  withProgress(message = "Reading TurboVeg XML", expr = {
    progress_mod  = ceiling(length(plot_nodes)/min(length(plot_nodes), 10))
    progress_incr = 1 / min(length(plot_nodes), 10)
    
    # Header data - Standard records
    header_std_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.25, message = "Header data (standard)")}
      plot_children = plot_nodes[i] %>% xml_children()
      header_children = plot_children[which(xml_name(plot_children) == "header_data")] %>% xml_children()   
      header_children[which(xml_name(header_children) == "standard_record")] %>% xml_attrs() 
    }) %>% 
      bind_rows() %>%
      na_if("null") %>% 
      na_if("") %>% 
      select_if(~!all(is.na(.)))

    # Header data - Undefined records
    header_udf_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.25, message = "Header data (undefined)")}
      plot_children = plot_nodes[i] %>% xml_children()
      header_children = plot_children[which(xml_name(plot_children) == "header_data")] %>% xml_children() 
      udf_names = header_children[which(xml_name(header_children) == "udf_record")] %>% xml_attr("name") 
      udf_vals = header_children[which(xml_name(header_children) == "udf_record")] %>% xml_attr("value") 
      bind_rows(setNames(c(xml_attr(plot_nodes[i], "releve_nr"), udf_vals), c("releve_nr", udf_names)))
    }) %>% 
      bind_rows() %>%
      na_if("null") %>% 
      na_if("") %>% 
      select_if(~!all(is.na(.)))

    # ----------------------------------------------------------- #
    # Species data 
    species_std_records = lapply(seq_along(plot_nodes), function(i){
      if(i %% progress_mod == 0){incProgress(progress_incr * 0.4, message = "Species data")}
      plot_children = plot_nodes[i] %>% xml_children()
      species_data_children = plot_children[which(xml_name(plot_children) == "species_data")] %>% xml_children()
      species_children = species_data_children[which(xml_name(species_data_children) == "species")] %>% xml_children()
      standard_records = species_children[which(xml_name(species_children) == "standard_record")] %>% 
        xml_attrs() %>% 
        bind_rows() %>% 
        mutate(releve_nr = xml_attr(plot_nodes[i], "releve_nr"))
    }) %>% 
      bind_rows()

    # ----------------------------------------------------------- #
    # Lookup tables ####
    lookup_names = tv_xml %>% xml_child("Lookup_tables") %>% xml_children() %>% xml_name()
    
    incProgress(0.05, message = "Lookup tables")
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
  })
  
  tv_import = list(std_header = header_std_records,
                   udf_header = header_udf_records,
                   species = species_std_records,
                   lookup = lookup_dfs)
  return(tv_import)
}

