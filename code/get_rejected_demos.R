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
  mutate(covid_reject = )

num_reject <- reject_summary %>% pull(id) %>% unique() %>% length()

covid_rej <- reject_summary %>% 
  filter(str_detect(response, "Covid")==TRUE) 

num_covid_rej <- covid_rej %>% 
  pull(id) %>% unique() %>% length()

percent_covid_rej <- get_percent(num_covid_rej, num_reject)

#data set w/ T/F
covid_rej_ids <- covid_rej %>% 
  select(id) %>% 
  mutate(covid_reject = "true")

reject_tf_summary <- left_join(reject_summary, covid_rej_ids, by = "id") %>% 
  mutate(covid_reject = replace(covid_reject, is.na(covid_reject), "false")) %>% 
  left_join(., demographics, by = "id")

#Gender - caregiving? kids?     


#Racial Minority


#Academic Field


#Position (PhD, postdoc, other)