#### Preparations
# VegX schema
schema_files = load_schema()
vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)

# other
log_path = tempfile("log_", fileext = ".csv")
new_action_log_record(log_path, "System info", "Test started.", append = F, col.names = T)

#------------------------------------------------------------#

test_that("Returns no warnings and errors with correct input", {
  node_paths = c("method > name", "method > description")
  node_values = c("foo", "bar")

  fct_result = new_vegx_node(node_paths, node_values, id = NULL, log_path, vegx_schema)
  expect_s3_class(fct_result$node, "xml_document")
  expect_equal(fct_result$warnings, 0)
  expect_equal(fct_result$errors, 0)
})

#------------------------------------------------------------#

test_that("Returns NULL node and an error with incorrect input", {
  node_paths = c("method > name", "project > name")
  node_values = c("foo", "bar")

  fct_result = new_vegx_node(node_paths, node_values, id = NULL, log_path, vegx_schema)
  expect_null(fct_result$node)
  expect_equal(fct_result$warnings, 0)
  expect_equal(fct_result$errors, 1)
})