library(tidyverse)
library(readxl)

hiring_data_raw <- read_excel("data/pandemic_hiring_survey_2021_13.20.xlsx") %>% 
  select(-c(1:17)) %>% 
  rowid_to_column()

colnames(hiring_data_raw) <- c("id", "institution_type", "university", "dept_name", "in_medical_school",
                             "hiring_q", "open_ten_track", "open_non_ten_track", "not_hiring_reasons")
  
hiring_data <- hiring_data_raw %>% 
  filter(!is.na(institution_type)) %>% 
  mutate(open_ten_track = replace_na(open_ten_track, 0),
         open_non_ten_track = replace_na(open_non_ten_track, 0),
         location = if_else(str_detect(university, "McM|McG"), "Canada", "USA")) %>% 
  filter(open_ten_track != "-4")

inst_summary <- hiring_data %>% 
  group_by(location, institution_type, in_medical_school) %>% 
  summarise(n = n())

hiring_summary <- hiring_data %>% 
  group_by(hiring_q, not_hiring_reasons) %>% 
  summarise(n = n(), hiring_ten_track = sum(open_ten_track),
            hiring_non_ten_track = sum(open_non_ten_track))
