#explore demographics of offers rescinded due to covid
#requires data from "get_offers_data.R"

offers_made <- offers_data %>% pull(faculty_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made

#Gender, race, field, position
res_demo_data <- left_join(offers_data, demographics, by = "id") %>% 
  filter(faculty_offers > 0) %>% 
  mutate(covid_offers_rescinded = 
           if_else(is.na(covid_offers_rescinded)|covid_offers_rescinded == 0, "false", "true"))

#Gender

gender_res_plot <- res_demo_data %>% 
  select(gender, covid_offers_rescinded, id) %>% 
  group_by(gender, covid_offers_rescinded) %>% 
  summarise(n=n()) %>% 
  spread(key = covid_offers_rescinded, value = n) %>% 
  mutate(percent_res = get_percent(true, false)) %>%
  ggplot()+
  geom_col(aes(x = gender, y=percent_res))

#race

race_res_plot <- res_demo_data %>% 
  mutate(race_ethnicity = fct_lump(race_ethnicity, n=4)) %>% 
  select(race_ethnicity, covid_offers_rescinded, id) %>% 
  group_by(race_ethnicity, covid_offers_rescinded) %>% 
  summarise(n=n()) %>% 
  spread(key = covid_offers_rescinded, value = n) %>% 
  mutate(percent_res = get_percent(true, false)) %>%
  ggplot()+
  geom_col(aes(x = race_ethnicity, y = percent_res))+
  coord_flip()+
  theme(#axis.text.x = element_text(angle = 90),
        legend.position = "none")

#field
field_res_plot <- get_plot_summary(res_demo_data, "research_category", "covid_offers_rescinded") %>% 
  ggplot()+
  geom_col(aes(x = research_category, y = percent_res))+
  coord_flip()+
  theme(legend.position = "none")

#position
position_res_plot <- res_demo_data %>% 
  select(position, covid_offers_rescinded, id) %>% 
  group_by(position, covid_offers_rescinded) %>% 
  summarise(n=n()) %>% 
  spread(key = covid_offers_rescinded, value = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(percent_res = get_percent(true, false)) %>% 
  ggplot()+
  geom_col(aes(x = position, y = percent_res))+
  coord_flip()+
  theme(legend.position = "none")
