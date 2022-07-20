#### Preparations
# VegX schema
schema_files = load_schema()
vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)

# Initialize an ID factory with one id_generator per ID name as defined by id_lookup (see /data-raw)
id_factory = sapply(unique(id_lookup), function(x){
  id_generator()
}, simplify = F)

#------------------------------------------------------------#
test_that("template node_ids are numbered correctly", {
  for(id in unique(templates_predefined$template_id)){
    template = templates_predefined %>% dplyr::filter(template_id == id)
    expect_equal(max(template$node_id), length(unique(template$node_id)))
  }
})

test_that("attributes get ids", {
  template = templates_predefined %>% dplyr::filter(template_id == 1) 
  result = templates_to_nodes(template, vegx_schema, id_factory)

  expect_true(xml2::xml_has_attr(result$methods[[1]], "id"))
  expect_true(xml2::xml_has_attr(result$attributes[[1]], "id"))
})

test_that("templates can be built", {
  build_nodes = function(id, id_factory){
    template = templates_predefined %>% dplyr::filter(template_id == id)
    templates_to_nodes(template, vegx_schema, id_factory)
  }
  
  for(id in unique(templates_predefined$template_id)){
    expect_silent(build_nodes(!!id, id_factory))
  }
})


