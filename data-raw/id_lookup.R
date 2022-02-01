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

usethis::use_data(id_lookup, overwrite = TRUE)
