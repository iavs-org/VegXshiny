#### Preparations
# VegX schema
schema_files = load_schema()
vegx_schema = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema, "veg", schema_files, simplify = T)


test_that("Subnodes are built only when all required fields have values", {
  nodes_df = data.frame("plot > plotName" = LETTERS[1:4],
                        "plot > geometry > area > value" = c(5,5,NA,5),
                        "plot > geometry > area > attributeID" = 1,
                        check.names = F)
 
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  leaf_nodes = sapply(fct_result, function(node){length(xml_find_all(node$node, ".//*[not(*)]"))})
  expect_equal(leaf_nodes, c(3,3,1,3))
})


#------------------------------------------------------------#
test_that("Returns the correct number of nodes", {
  nodes_df = data.frame("party > choice > individualName" = "A",
                        "party > address" = "B",
                        "party > phone" = "C",
                        "party > electronicMailAddress" = "D",
                        "party > onlineURL" = "E",
                        check.names = F)
  
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  
  expect_equal(length(fct_result), 1)
  expect_equal(xml_length(fct_result[[1]]$node), 5)
})

#------------------------------------------------------------#
test_that("Node order always conforms to schema (depth = 1)", {
  nodes_df = data.frame("party > choice > individualName" = "A",
                        "party > address" = "B",
                        "party > phone" = "C",
                        "party > electronicMailAddress" = "D",
                        "party > onlineURL" = "E",
                        check.names = F)
  
  names_ref = c("individualName",  "address", "phone", "electronicMailAddress", "onlineURL") # correct order according to schema
  
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
  
  fct_result = new_vegx_nodes(nodes_df[, c(5,4,3,2,1)], vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
  
  fct_result = new_vegx_nodes(nodes_df[, c(4,3,5,1,2)], vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
})

test_that("Node order always conforms to schema (depth > 1)", {
  nodes_df = data.frame("plot > plotName" = "A",
                        "plot > location > horizontalCoordinates > coordinates > valueX" = "B",
                        "plot > location > authorLocation" = "C",
                        "plot > topography > aspect > value" = "D",
                        "plot > simpleUserDefined > choice > methodID" = "E",
                        check.names = F)
  
  names_ref = c("plotName", "valueX", "authorLocation","value", "methodID") # correct order according to schema
  
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
  
  fct_result = new_vegx_nodes(nodes_df[, c(5,4,3,2,1)], vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
  
  fct_result = new_vegx_nodes(nodes_df[, c(4,3,5,1,2)], vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(names_ref, names_test)
})

test_that("Invalid node names are handled properly", {
  nodes_df = data.frame("method > invalidNodeName" = "A", check.names = F)
  expect_error(new_vegx_nodes(nodes_df, vegx_schema), "No valid node paths")

  nodes_df = data.frame("method > name" = "A",
                        "method > invalidNodeName" = "B",
                        check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(length(names_test), 1)
  expect_equal(names_test, "name")
  
  nodes_df = data.frame("method > invalidNodeName" = "A",
                        "method > description" = "B",
                        check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  names_test = fct_result[[1]]$node %>% xml_find_all(".//*[not(*)]") %>% xml_name()
  expect_equal(length(names_test), 1)
  expect_equal(names_test, "description")
})

test_that("Missing node values are handled properly", {
  nodes_df = data.frame("party > choice > individualName" = "A",
                        "party > address" = NA,
                        check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_equal(xml_length(fct_result[[1]]$node), 1)
  
  nodes_df = data.frame("party > choice > individualName" = c("A", "B"),
                        "party > address" = c("C", NA),
                        check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_equal(xml_length(fct_result[[1]]$node), 2)
  expect_equal(xml_length(fct_result[[2]]$node), 1)
})

test_that("Nodes without children are handled properly", {
  nodes_df = data.frame("organismName" = c("A", "B"))
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_equal(length(fct_result), 2)
  expect_equal(xml_text(fct_result[[1]]$node), "A")
  expect_equal(xml_text(fct_result[[2]]$node), "B")
})

test_that("Element types are checked properly", {
  nodes_df = data.frame("plotObservation > obsStartDate" = "this is not a date", check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_null(fct_result[[1]])

  nodes_df = data.frame("attribute > choice > quantitative > precision" = "this is not a decimal", check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_null(fct_result[[1]])
  
  nodes_df = data.frame("stratum > order" = "this is not an integer", check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_null(fct_result[[1]])
  
  nodes_df = data.frame("plotObservation > obsStartDate" = c("20021212", "this is not a date"), 
                        "plotObservation > plotID" = c("1", "2"), 
                        check.names = F)
  fct_result = new_vegx_nodes(nodes_df, vegx_schema)
  expect_equal(length(fct_result), 2)
  expect_equal(xml_length(fct_result[[1]]$node), 2)
  expect_equal(xml_length(fct_result[[2]]$node), 1)
})