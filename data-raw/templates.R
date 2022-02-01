library(dplyr)
library(stringr)
library(tidyr)

# prepare data

methods_raw = read.csv("data-raw/methods_overview.csv", stringsAsFactors = F) %>% 
  dplyr::select(-default) %>% 
  mutate(subject = replace(subject, str_detect(subject, "Climate"), "climate"),
         template_id = 1:nrow(.))

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

# Export datasets
usethis::use_data(templates, overwrite = TRUE)