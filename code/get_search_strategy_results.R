library(tidyverse)
library(readxl)

strategy_data_raw <- read_excel("data/search_strategy_survey_2021_13.21.xlsx") %>% 
  filter(Progress == 100) %>% 
  select(-c(1:17)) %>% 
  rowid_to_column()

colnames(strategy_data_raw) <- c("id", "current_app", "current_position", "research_category",
                             "desired_institution", "altered_research_remote", "altered_teaching_online",
                             "altered_research_covid", "no_inperson_interv", "covid_restrict_geog",
                             "covid_dep_offer", "no_search_reasons", "change_career_reasons")
  
strategy_data_raw <- strategy_data_raw[-8,]

extent_factors <- c("Strongly disagree", "Disagree", "Somewhat disagree", 
                    "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")

strategy_data <- strategy_data_raw %>% 
  mutate(reasons = coalesce(no_search_reasons, change_career_reasons),
         reasons = str_replace_all(reasons, ",(?=.*?\\))", ";"),
         reasons = str_replace(reasons, "(?<=\\));", ",")) %>% 
  separate(reasons, 
           into = c("a", "b", "c", "d", "e", "f"), sep = ",") %>% 
  gather(a:f, key = "y", value = "reasons") %>% 
  gather(altered_research_remote:covid_dep_offer, key = "concern", value = "extent") %>% 
  select(-y, -no_search_reasons, -change_career_reasons) %>% 
  mutate(reasons = if_else(is.na(reasons), "none provided", reasons))
  

respondent_summary <- strategy_data %>% 
  group_by(current_app, current_position, research_category) %>% 
  summarise(n = n())

no_summary <- strategy_data %>% 
  filter(str_detect(current_app, "No") == TRUE) %>% 
  group_by(current_app, reasons) %>% 
  summarise(n = n())

yes_summary <- strategy_data %>% 
  filter(current_app == "Yes") %>% 
  group_by(concern, extent) %>% 
  summarise(n = n())

offer_summary <- yes_summary %>% 
  filter(concern == "covid_dep_offer")

yes_sum_plot <- yes_summary %>% 
  filter(concern != "covid_dep_offer") %>% 
  as_tibble() %>% 
  mutate(extent = factor(extent, extent_factors)) %>% 
  ggplot() +
  geom_col(aes(x = fct_rev(extent), y = n)) +
  facet_wrap(~concern) +
  coord_flip()
