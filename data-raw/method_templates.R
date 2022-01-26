library(dplyr)
library(stringr)
library(tidyr)

# prepare data

methods_raw = read.csv("data-raw/methods_overview.csv", stringsAsFactors = F) %>% 
  dplyr::select(-default) %>% 
  mutate(subject = replace(subject, str_detect(subject, "Climate"), "climate"),
         template_id = 1:nrow(.))

methods_long = methods_raw %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = -template_id, names_to = "node_path", values_to = "node_value")

templates = methods_long %>% 
  drop_na() %>% 
  mutate(main_element = ifelse(node_path %in% c("name", "subject", "description"), "methods", "attributes")) %>% 
  relocate(main_element, .after = template_id) %>% 
  mutate(node_path = recode(node_path,
                            `name` = "method > name",
                            `description` = "method > description",
                            `subject` = "method > subject",
                            `unit` = "attribute > quantitative > unit",
                            `lowerLimit` = "attribute > quantitative > lowerLimit",
                            `upperLimit` = "attribute > quantitative > upperLimit",)) %>% 
  mutate(node_item = 1)

templates_overview = methods_raw %>% 
  mutate(target_element = "methods") %>% 
  dplyr::select(template_id, target_element, subject, name) %>% 
  distinct()

# Export datasets
usethis::use_data(templates, overwrite = TRUE)
usethis::use_data(templates_overview, overwrite = TRUE)

