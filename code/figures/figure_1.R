#Figure 1. The Pandemic begins, mid-interview portion of the Faculty Job Search

# A. Offers Rescinded by field and gender----
# requires get_offers_data 
gender_res_plot <- get_plot_summary(res_demo_data, "gender", "covid_offers_rescinded") %>% 
  ggplot()+
  geom_col(aes(x = gender, y=percent_res))+
  labs(x = "\nReported Gender", y = "Percent of Offers Rescinded",
       caption = "N = Number of respondants who recieved an offer")+
  my_theme_horiz

field_res_plot <- get_plot_summary(data = res_demo_data, 
                                   x = "research_category", y = "covid_offers_rescinded") %>% 
  ggplot()+
  geom_col(aes(x = research_category, y = percent_res))+
  coord_flip()+
  labs(y = "Percent of Offers Rescinded", x = "Reported Research Category")+
  my_theme

# B. Offers rescinded by race/ethnicity vs visa status----
race_res_plot <- race_data %>% 
  group_by(spons_req, race_ethnicity, covid_offers_rescinded) %>% 
  summarise(n = n()) %>% 
  as_tibble() %>% 
  spread(key = covid_offers_rescinded, value = n) %>% 
  mutate(total = true + false,
         total = if_else(is.na(total), "0", as.character(total)),
         percent = get_percent(true, total),
         race_ethnicity = paste0(race_ethnicity, "\n(n=", total, ")")) %>% 
  filter(total != 0) 

race_visa_plot <- race_res_plot%>% 
  #mutate(race_ethnicity = fct_lump(race_ethnicity, n=4)) %>% 
  #get_plot_summary(., "race_ethnicity", "covid_offers_rescinded") %>% 
  ggplot()+
  geom_col(aes(x = fct_reorder(race_ethnicity, desc(percent)), y = percent, fill = spons_req))+
  coord_flip(ylim = c(0,50))+
  #facet_wrap(~legal_status, ncol = 1, scales = "free_y")+
  labs(y = "\nPercent of Offers Rescinded", x = "Reported Race/Ethnicity",
       fill = "Visa Sponsor\nRequired")+
  my_theme_leg_horiz+
  theme(legend.position = c(.8,.5), legend.text =element_text(size=12),
        legend.title = element_text(size = 12))

# C. Compare the % of R1 vs PUI applications submitted vs offers rescinded----
submitted <- app_outcomes %>% 
  select(id, R1_apps_submitted, PUI_apps_submitted) %>% 
  mutate(R1_apps_submitted = if_else(is.na(R1_apps_submitted), "0", R1_apps_submitted),
         PUI_apps_submitted = if_else(is.na(PUI_apps_submitted), "0", PUI_apps_submitted),
         total_apps_submitted = as.numeric(R1_apps_submitted) + as.numeric(PUI_apps_submitted))
  

# D. Compare region of institutions applied to and the number of offers rescinded----