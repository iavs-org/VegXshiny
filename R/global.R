load("R/sysdata.rda")

# --------------------------------------------------------------------------------------- #
####      VegX schema preparation      ####
# --------------------------------------------------------------------------------------- #
# Link and simplify schema
schema_files = load_schema() # read fresh copy of schema files
vegx_schema_simple = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema_simple, "veg", schema_files, simplify = T)

# Collect VegX main element names
vegx_main_elements = xml2::xml_attr(xml_children(vegx_schema_simple), "name")

# Collect VegX leaf element names
vegx_leaf_elements = xml_find_all(vegx_schema_simple, "//*[@name='vegX']//*[not(*)]") %>%
  xml2::xml_attr("name") %>%
  unique()
vegx_leaf_elements = vegx_leaf_elements[!is.na(vegx_leaf_elements)]

# --------------------------------------------------------------------------------------- #
####    ID factory     ####
# --------------------------------------------------------------------------------------- #
# Initialize an ID factory with one id_generator per ID name as defined by id_lookup (see /data-raw)
id_factory = sapply(unique(id_lookup), function(x){
  id_generator()
}, simplify = F)
