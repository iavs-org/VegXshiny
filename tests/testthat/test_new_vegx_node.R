setwd("../..") # Set wd to package root dir
token = "test"

test_that("Returns no warnings and errors with correct input", {
  # Test set 1
  node_paths = c("method > name", "method > description")
  node_values = c("foo", "bar")
  
  fct_result = new_vegx_node(node_paths, node_values, id = NULL, token)
  expect_s3_class(fct_result$node, "xml_document")
  expect_equal(fct_result$warnings, 0)
  expect_equal(fct_result$errors, 0)
})

#------------------------------------------------------------#

test_that("Returns NULL node and an error with incorrect input", {
  node_paths = c("method > name", "project > name")
  node_values = c("foo", "bar")
  
  fct_result = new_vegx_node(node_paths, node_values, id = NULL, token)
  expect_null(fct_result$node)
  expect_equal(fct_result$warnings, 0)
  expect_equal(fct_result$errors, 1)
})

#------------------------------------------------------------#
# TODO: No ID conflicts when mixing auto and user-supplied ids