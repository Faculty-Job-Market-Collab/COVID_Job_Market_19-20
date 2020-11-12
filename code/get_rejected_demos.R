#explore demographics of applicants that rejected offers due to covid?
#requires data from "get_offers_data.R"

offer_response_data <- offers_data %>% select(id, offer_responses) %>% 
  mutate(num_resp = str_count(offer_responses, ",")) %>% 
  separate(., offer_responses, sep = ",", 
           into = c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")) %>% 
  gather(resp1:resp6, key = num, value = response) %>% 
  filter(!is.na(response)) %>% 
  select(-num_resp, -num)

#max_resp <- max(offer_response_data$num_resp, na.rm = TRUE) + 1 #use to calculate max num of responses to expect, add one b/c last response will not have a comma

response_summary <- offer_response_data %>% 
  group_by(response) %>% summarise(n = n())


#% of overall rejections
reject_summary <- offer_response_data %>% 
  group_by(id, response) %>% summarise(n = n()) %>% 
  filter(str_detect(response, "reject") == TRUE) %>% 
  mutate(covid_reject = if_else(str_detect(response, "Covid"), "true", "false"))

num_reject <- reject_summary %>% pull(id) %>% unique() %>% length()

covid_rej <- reject_summary %>% 
  filter(covid_reject=="true") 

num_covid_rej <- covid_rej %>% 
  pull(id) %>% unique() %>% length()

percent_covid_rej <- get_percent(num_covid_rej, num_reject)

#data set w/ demo data
reject_summary_demo <- left_join(reject_summary, demographics, by = "id")

#Gender   
get_plot_summary(reject_summary_demo, "gender", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = gender, y=percent_res))

#Dependents
get_plot_summary(reject_summary_demo, "dependents", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = dependents, y=percent_res))

#primary caregiver
get_plot_summary(reject_summary_demo, "primary_caregiver", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = primary_caregiver, y=percent_res))+
  coord_flip()

#Racial Minority
reject_summary_demo %>% 
  get_plot_summary(., "race_ethnicity", "covid_reject") %>% 
  arrange(desc(percent_res)) %>% 
  head(n = 4) %>% 
    ggplot()+
  geom_col(aes(x = race_ethnicity, y = percent_res))+
  coord_flip()+
  theme(#axis.text.x = element_text(angle = 90),
    legend.position = "none")

#income
get_plot_summary(reject_summary_demo, "income", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = income, y=percent_res))+
  coord_flip()

#student loan
get_plot_summary(reject_summary_demo, "student_loan", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = student_loan, y=percent_res))+
  coord_flip()

#disability status
get_plot_summary(reject_summary_demo, "disability_status", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = disability_status, y=percent_res))+
  coord_flip()

#age
get_plot_summary(reject_summary_demo, "age", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = age, y=percent_res))+
  coord_flip()

#relationship status
get_plot_summary(reject_summary_demo, "relationship_status", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = relationship_status, y=percent_res))+
  coord_flip()

#partner occupation
get_plot_summary(reject_summary_demo, "partner_occupation", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = partner_occupation, y=percent_res))+
  coord_flip()

#financial support
get_plot_summary(reject_summary_demo, "financial_support", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = financial_support, y=percent_res))+
  coord_flip()

#resident
get_plot_summary(reject_summary_demo, "residence", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = residence, y=percent_res))+
  coord_flip()

#legal status
get_plot_summary(reject_summary_demo, "legal_status", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = legal_status, y=percent_res))+
  coord_flip()

#first gen undergrad
get_plot_summary(reject_summary_demo, "first_gen_undergrad", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = first_gen_undergrad, y=percent_res))

#first gen phd
get_plot_summary(reject_summary_demo, "first_gen_phd", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = first_gen_phd, y=percent_res))

#Academic Field
get_plot_summary(reject_summary_demo, "research_category", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=percent_res))+
  coord_flip()

get_plot_summary(reject_summary_demo, "biomedical", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = biomedical, y=percent_res))+
  coord_flip()

#Position (PhD, postdoc, other)
get_plot_summary(reject_summary_demo, "position", "covid_reject") %>% 
  ggplot()+
  geom_col(aes(x = position, y=percent_res))+
  coord_flip()
