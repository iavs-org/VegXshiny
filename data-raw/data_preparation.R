library(dplyr)
library(stringr)
library(tidyr)
library(xml2)

#### Templates lookup ####
methods_raw = read.csv("data-raw/methods_overview.csv", stringsAsFactors = F) %>% 
  dplyr::select(-default) %>% 
  mutate(subject = replace(subject, str_detect(subject, "Climate"), "climate"),
         template_id = 1:nrow(.))

templates_lookup = methods_raw %>% 
  mutate(target_element = "methods") %>% 
  dplyr::select(template_id, target_element, subject, name, description) %>% 
  distinct()

#### Templates ####
methods_long = methods_raw %>% 
  mutate(across(-template_id, as.character)) %>% 
  pivot_longer(cols = -template_id, names_to = "node_path", values_to = "node_value")

templates = methods_long %>% 
  drop_na() %>% 
  mutate(main_element = ifelse(node_path %in% c("name", "subject", "description"), "methods", "attributes"),
         node_path = recode(node_path,
                            `name` = "method > name",
                            `description` = "method > description",
                            `subject` = "method > subject",
                            `unit` = "attribute > quantitative > unit",
                            `lowerLimit` = "attribute > quantitative > lowerLimit",
                            `upperLimit` = "attribute > quantitative > upperLimit"),
         node_id = recode(main_element,
                          `methods` = 1, 
                          `attributes` = 2)) %>% 
  relocate(node_id, main_element, .after = template_id)

templates = templates %>% 
  group_by(template_id) %>% 
  group_modify(~ add_row(.x, node_id = 2, main_element = "attributes", node_path = "attribute > quantitative > methodID", node_value = "1"))

#### ID_lookup ####
# Get unique ID names in schema
# vegx_schema_simple = read_xml("~/Documents/schema_simple.xml")
# ID_names = xml_find_all(vegx_schema_simple, "//*[@name]") %>% 
#   xml_attr("name") %>% 
#   unique() %>% 
#   stringr::str_subset("ID$")

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
  "partyID"                         = "partyID",
  "determinationPartyID"            = "partyID",
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

#### Export data ####
usethis::use_data(id_lookup, templates_lookup, templates, overwrite = TRUE, internal = TRUE)
