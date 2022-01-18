# Namespace definitions
namespace_uris = list(
  "http://iavs.org/vegX/misc-2.0.1" = "misc",
  "http://iavs.org/vegX/plotobservation-2.0.1" = "obs", 
  "http://iavs.org/vegX/plot-2.0.1" = "plot",
  "http://iavs.org/vegX/organism-2.0.1" = "org",
  "http://iavs.org/vegX/community-2.0.1" = "comm"
)

# ------------------------------------- #
####       Schema preparation        ####
# ------------------------------------- #
# Link schema
schema_files = load_schema() # read schema files
vegx_schema_full = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema_full, "veg", schema_files, simplify = F)

# Simplify linked schema
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

# ------------------------------------- #
####    VegX document preparation    ####
# ------------------------------------- #
# Create empty VegX file
vegx_doc = xml_new_root("vegX")

# create namespace attributes
ns_uris = unlist(namespace_uris)
attrs = setNames(names(ns_uris), paste0("xmlns:", ns_uris))
xml_set_attrs(vegx_doc, attrs)