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
# Link schema
schema_files = load_schema() # read schema files
vegx_schema_full = xml2::xml_find_all(schema_files[["veg"]], ".//*[@name='vegX']")
link_vegx_schema(vegx_schema_full, "veg", schema_files, simplify = F)
write_xml(vegx_schema_full, "~/Documents/schema.xml")

# Simplify linked schema
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
attrs = setNames(names(ns_uris), paste0("xmlns:", ns_uris))
xml_set_attrs(vegx_doc, attrs)

# --------------------------------------------------------------------------------------- #
####    ID factory     ####
# --------------------------------------------------------------------------------------- #
# Get unique ID names in schema
ID_names = xml_find_all(vegx_schema_simple, "//*[@name]") %>% 
  xml_attr("name") %>% 
  unique() %>% 
  stringr::str_subset("ID$")

# Build lookup table to match 
id_lookup = c(
  "aggregateOrganismObservationID"  = "aggregateOrganismObservationID",
  "attributeID"                     = "attributeID",                 
  "qualitativeAttributeID"          = "attributeID",
  "citationID"                      = "citationID",
  "accordingToCitationID"           = "citationID",
  "documentCitationID"              = "citationID",
  "interpretationCitationID"        = "citationID",
  "communityConceptID"              = "communityConceptID",
  "CommunityDetermination"          = "communityDeterminationID",
  "communityObservationID"          = "communityObservationID",
  "individualOrganismID"            = "individualOrganismID",
  "relatedIndividualID"             = "individualOrganismID",
  "individualOrganismObservationID" = "individualOrganismObservationID",
  "literatureCitationID"            = "literatureCitationID",
  "measurementID"                   = "measurementID",
  "methodID"                        = "methodID",
  "qualityAssessmentMethodID"       = "methodID",
  "observationGroupingID"           = "observationGroupingID",
  "organismIdentityID"              = "organismIdentityID",          
  "organismNameID"                  = "organismNameID",
  "originalOrganismNameID"          = "organismNameID",
  "determinationPartyID"            = "partyID",
  "partyID"                         = "partyID",
  "originalIdentificationPartyID"   = "partyID",
  "interpretationPartyID"           = "partyID",
  "conceptAssertionPartyID"         = "partyID",
  "placementPartyID"                = "partyID",
  "locationPartyID"                 = "partyID",
  "elevationPartyID"                = "partyID",
  "depthPartyID"                    = "partyID",
  "observationPartyID"              = "partyID",
  "plotID"                          = "plotID",
  "protocolID"                      = "protocolID",
  "relatedPlotID"                   = "plotID",
  "plotObservationID"               = "plotObservationID",
  "previousPlotObservationID"       = "plotObservationID",    
  "projectID"                       = "projectID",
  "relatedProjectID"                = "projectID",
  "protocolID"                      = "protocolID",
  "relatedItemID"                   = "relatedItemID",
  "taxonConceptID"                  = "taxonConceptID",
  "taxonDetermination"              = "taxonDeterminationID",
  "taxonNameID"                     = "taxonNameID",
  "preferredTaxonNameID"            = "taxonNameID",
  "taxonNameUsageConceptID"         = "taxonNameUsageConceptID",
  "siteObservationID"               = "siteObservationID",
  "stratumID"                       = "stratumID",
  "stratumObservationID"            = "stratumObservationID",
  "surfaceCoverObservationID"       = "surfaceCoverObservationID",
  "surfaceTypeID"                   = "surfaceTypeID",
  "vegXID"                          = "vegxID"
)

# Define a generic id generator function that increments its value every time it is called
id_generator = function(value = 0){
  function(){
    value <<-value+1
    return(value)
  }
}

# Initialize an ID factory with one id_generator per "true" ID name
id_factory = sapply(unique(id_lookup), function(x){
  id_generator()
}, simplify = F)
