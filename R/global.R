# Namespace definitions
namespace_uris = list(
  "http://iavs.org/vegX/misc-2.0.1" = "misc",
  "http://iavs.org/vegX/plotobservation-2.0.1" = "obs", 
  "http://iavs.org/vegX/plot-2.0.1" = "plot",
  "http://iavs.org/vegX/organism-2.0.1" = "org",
  "http://iavs.org/vegX/community-2.0.1" = "comm"
)

# Namespace file locations
namespaces = list(
  veg  = read_xml("inst/app/www/vegxschema/veg.xsd"),
  misc = read_xml("inst/app/www/vegxschema/veg-misc.xsd"),
  obs  = read_xml("inst/app/www/vegxschema/veg-plotobservation.xsd"),
  plot = read_xml("inst/app/www/vegxschema/veg-plot.xsd"),
  org  = read_xml("inst/app/www/vegxschema/veg-organism.xsd"),
  comm = read_xml("inst/app/www/vegxschema/veg-community.xsd")
)

# VegX Schema
vegx_smpl = xml_find_all(namespaces[["veg"]], ".//*[@name='vegX']")
simplify_vegx_node(vegx_smpl, "veg", namespaces)

# VegX names
vegx_names = xml_attr(xml_children(vegx_smpl), "name")