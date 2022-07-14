#### Preparations
# VegX schema
schema_files = load_schema()
vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)

#------------------------------------------------------------#
test_that("template node_ids are numbered correctly", {
  for(id in unique(templates_predefined$template_id)){
    template = templates_predefined %>% dplyr::filter(template_id == id)
    expect_equal(max(template$node_id), length(unique(template$node_id)))
  }
})

test_that("templates can be built", {
  build_nodes = function(id){
    template = templates_predefined %>% dplyr::filter(template_id == id)
    templates_to_nodes(template, vegx_schema, tmp_dir(), write_log = F)
  }
  
  for(id in unique(templates_predefined$template_id)){
    expect_silent(build_nodes(!!id))
  }
})
