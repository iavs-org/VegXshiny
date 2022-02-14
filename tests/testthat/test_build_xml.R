test_that("Invalid node names are handled properly", {
  root = xml_new_root("method")
  expect_warning(build_xml(root, list(c("method","invalidNodeName")), "foo"), "invalid node name")
  expect_equal(length(xml_children(root)), 0)
  
  expect_warning(build_xml(root, list(c("method","name"), c("method","invalidNodeName")), c("foo", "bar")), "invalid node name")
  expect_equal(length(xml_children(root)), 1)
  
  expect_warning(build_xml(root, list(c("method","invalidNodeName"), c("method","description")), c("foo", "bar")), "invalid node name")
  expect_equal(length(xml_children(root)), 2)
})

test_that("Node placement complies with schema definition", {
  # Check at depth = 1 
  node_paths = list(c("party","choice", "individualName"), c("party", "address"), c("party","phone"), c("party", "electronicMailAddress"), c("party", "onlineURL"))
  names_ref = c("individualName",  "address", "phone", "electronicMailAddress", "onlineURL") # correct order according to schema
  
  for(i in 1:10){
    root = xml_new_root("party")
    build_xml(root, sample(node_paths, length(node_paths)), letters[1:length(node_paths)])
    names_test = root %>% xml_find_all(".//*[not(*)]") %>% xml_name()
    expect_equal(names_ref, names_test)
  }
  
  # Check at depths > 1 
  node_paths = list(c("plot", "plotName"), c("plot", "location", "horizontalCoordinates", "coordinates", "valueX"),  c("plot", "location", "authorLocation"), 
                    c("plot", "topography", "aspect", "value"), c("plot", "simpleUserDefined", "choice", "methodID"))
  names_ref = c("plotName", "valueX", "authorLocation","value", "methodID") # correct order according to schema
  
  for(i in 1:10){
    root = xml_new_root("plot")
    build_xml(root, sample(node_paths, length(node_paths)), letters[1:length(node_paths)])
    names_test = root %>% xml_find_all(".//*[not(*)]") %>% xml_name()
    expect_equal(names_ref, names_test)
  }
})

#------------------------------------------------------------#

test_that("MaxOccurs complies with schema definition", {
  root = xml_new_root("method")
  expect_warning(build_xml(root, list(c("method","name"), c("method", "name")), c("foo", "bar")), "maxOccurs reached")
  expect_equal(xml_text(xml_children(root)), "foo")
  
  root = xml_new_root("method")
  expect_warning(build_xml(root, list(c("method","name"), c("method", "description"), c("method", "name")), c("foo", "bar", "baz")), "maxOccurs reached")
  expect_equal(xml_text(xml_children(root)), c("foo", "bar"))
  
  root = xml_new_root("method")
  expect_warning(build_xml(root, list(c("method","name"), c("method", "name"), c("method", "description")), c("foo", "bar", "baz")), "maxOccurs reached")
  expect_equal(xml_text(xml_children(root)), c("foo", "baz"))
})


#------------------------------------------------------------#

test_that("Choice elements can have only one child", {
  root = xml_new_root("method")
  expect_warning(build_xml(root, list(c("method", "choice", "citationID"), c("method", "choice", "citationString")), c("foo", "bar")), "one node allowed")
  expect_equal(xml_text(xml_children(root)), "foo")
  
  root = xml_new_root("attribute")
  expect_warning(build_xml(root, list(c("attribute", "choice", "quantitative", "methodID"), c("attribute", "choice", "qualitative", "methodID")), c("foo", "bar")), "one node allowed")
  expect_equal(xml_text(xml_children(root)), "foo")
  
  root = xml_new_root("taxonDetermination")
  expect_warning(build_xml(root, list(c("taxonDetermination", "taxonRelationshipAssertion", "assertion", "choice", "taxonNameID"), 
                                      c("taxonDetermination", "taxonRelationshipAssertion", "assertion", "choice", "taxonConceptID")), c("foo", "bar")), "one node allowed")
  expect_equal(xml_text(xml_children(root)), "foo")
})

#------------------------------------------------------------#

test_that("Element types are checked properly", {
  root = xml_new_root("plotObservation")
  expect_warning(build_xml(root, list(c("plotObservation", "obsStartDate")), c("this is not a date")), "invalid date format")
  expect_equal(length(xml_children(root)), 0)
  
  root = xml_new_root("attribute")
  expect_warning(build_xml(root, list(c("attribute", "choice", "quantitative", "precision")), c("this is not a decimal")), "invalid decimal format")
  expect_equal(length(xml_children(root)), 0)
  
  root = xml_new_root("stratum")
  expect_warning(build_xml(root, list(c("stratum", "order")), c("this is not an integer")), "invalid integer format")
  expect_equal(length(xml_children(root)), 0)
})

