# Namespace definitions
namespace_uris = list(
  "http://iavs.org/vegX/misc-2.0.1" = "misc",
  "http://iavs.org/vegX/plotobservation-2.0.1" = "obs", 
  "http://iavs.org/vegX/plot-2.0.1" = "plot",
  "http://iavs.org/vegX/organism-2.0.1" = "org",
  "http://iavs.org/vegX/community-2.0.1" = "comm"
)

# --------------------------------------------------------------------------------------- #
####      VegX schema preparation      ####
# --------------------------------------------------------------------------------------- #
# Link and simplify schema
schema_files = load_schema() # read fresh copy of schema files
vegx_schema_simple = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema_simple, "veg", schema_files, simplify = T)
write_xml(vegx_schema_simple, "~/Documents/schema_simple.xml")

# Collect VegX main element names
vegx_main_elements = xml2::xml_attr(xml_children(vegx_schema_simple), "name")

# Collect VegX leaf element names
vegx_leaf_elements = xml_find_all(vegx_schema_simple, "//*[@name='vegX']//*[not(*)]") %>%
  xml2::xml_attr("name") %>%
  unique()
vegx_leaf_elements = vegx_leaf_elements[!is.na(vegx_leaf_elements)]

# --------------------------------------------------------------------------------------- #
####    VegX document preparation     ####
# --------------------------------------------------------------------------------------- #
# Create empty VegX file
vegx_doc = xml_new_root("vegX")

# create namespace attributes
ns_uris = unlist(namespace_uris)
ns_attrs_vegx = setNames(names(ns_uris), paste0("xmlns:", ns_uris))
ns_attrs_w3c = c("xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")
ns_attrs = c(ns_attrs_vegx, ns_attrs_w3c)
xml_set_attrs(vegx_doc, ns_attrs)

# --------------------------------------------------------------------------------------- #
####    ID factory     ####
# --------------------------------------------------------------------------------------- #
# Define a generic id generator function that increments its value every time it is called
id_generator = function(value = 0){
  function(){
    value <<-value+1
    return(value)
  }
}

# Initialize an ID factory with one id_generator per ID name as defined by id_lookup (see /data-raw)
id_factory = sapply(unique(id_lookup), function(x){
  id_generator()
}, simplify = F)
