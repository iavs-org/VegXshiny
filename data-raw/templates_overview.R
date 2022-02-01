library(dplyr)
library(stringr)
library(tidyr)

methods_raw = read.csv("data-raw/methods_overview.csv", stringsAsFactors = F) %>% 
  dplyr::select(-default) %>% 
  mutate(subject = replace(subject, str_detect(subject, "Climate"), "climate"),
         template_id = 1:nrow(.))

templates_overview = methods_raw %>% 
  mutate(target_element = "methods") %>% 
  dplyr::select(template_id, target_element, subject, name, description) %>% 
  distinct()

usethis::use_data(templates_overview, overwrite = TRUE)
