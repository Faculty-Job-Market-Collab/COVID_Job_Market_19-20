#did applicants alther their strategy?

strategy_data <- perceptions %>% 
  select(id, covid_alter_research)

strategy_summary <- strategy_data %>% 
  group_by(covid_alter_research) %>% summarise(n=n())

num_changed <- strategy_summary[3,2]

percent_changed <- (num_changed/sum(strategy_summary$n))*100
