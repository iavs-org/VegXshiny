test_that("Node placement complies with schema definition", {
  # common parameters
  node_values = c("foo", "bar", "baz")
  id = NULL
  session = list(token = "test")

  # Check at depth = 1
  names_ref1 = c("name", "subject", "citationID") # correct order according to schema
  names_test1 = new_vegx_node(c("methods > name", "methods > subject", "methods > choice > citationID"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test2 = new_vegx_node(c("methods > subject", "methods > name", "methods > choice > citationID"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test3 = new_vegx_node(c("methods > subject", "methods > choice > citationID", "methods > name"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test4 = new_vegx_node(c("methods > choice > citationID", "methods > subject", "methods > name"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()

  expect_equal(names_ref1, names_test1)
  expect_equal(names_ref1, names_test2)
  expect_equal(names_ref1, names_test3)
  expect_equal(names_ref1, names_test4)

  # Check at depth > 1
  names_ref2 = c("title", "partyID", "role") # correct order according to schema
  names_test5 = new_vegx_node(c("project > title", "project > personnel > partyID", "project > personnel > role"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test6 = new_vegx_node(c("project > personnel > partyID", "project > title", "project > personnel > role"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test7 = new_vegx_node(c("project > personnel > role", "project > title", "project > personnel > partyID"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()
  names_test8 = new_vegx_node(c("project > title", "project > personnel > role", "project > personnel > partyID"), node_values, id, session) %>% xml_find_all("//*[not(*)]") %>% xml_name()

  expect_equal(names_ref2, names_test5)
  expect_equal(names_ref2, names_test6)
  expect_equal(names_ref2, names_test7)
  expect_equal(names_ref2, names_test8)
})

#------------------------------------------------------------#

test_that("MaxOccurs complies with schema definition", {
  node = new_vegx_node(node_paths = c("methods > name", "methods > name"), node_values = c("foo", "bar"), id = NULL, session = list(token = "test"))
  expect_equal(xml_text(xml_children(node)), "foo")
})

#------------------------------------------------------------#

test_that("Choice elements can have only one child", {
  # Check at depth = 1
  node1 = new_vegx_node(node_paths = c("methods > choice > citationID", "methods > choice > citationString"), node_values = c("foo", "bar"), id = NULL, session = list(token = "test"))
  expect_equal(xml_text(xml_children(node1)), "foo")

  node2 = new_vegx_node(node_paths = c("attributes > choice > quantitative > methodID", "attributes > choice > qualitative > methodID"),
                        node_values = c("1", "2"), id = NULL, session = list(token = "test"))
  expect_equal(xml_text(xml_children(node2)), "1")

  # Check at depth > 1
  node3 = new_vegx_node(node_paths = c("taxonDetermination > taxonRelationshipAssertion > assertion > choice > taxonNameID", "taxonDetermination > taxonRelationshipAssertion > assertion > choice > taxonConceptID"),
                        node_values = c("1", "2"), id = NULL, session = list(token = "test"))
  expect_equal(xml_text(xml_children(node3)), "1")
})

#------------------------------------------------------------#

test_that("Node paths share the same root node (new_vegx_node)", {
  expect_error(new_vegx_node(node_paths = c("methods > name", "parties > phone"), node_values = c("foo", "bar"), id = NULL, session = list(token = "test")))
})