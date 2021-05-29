#how many offers were rescinded due to covid?

offers_data <- app_outcomes %>% 
  select(id, contains("offer")) %>% 
  mutate_at(c("faculty_offers", "covid_offers_rescinded"), as.numeric)

offers_made <- offers_data %>% pull(faculty_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made


#did applicants reject offers due to covid?

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

#12 offers rejected b/c of covid

offers_made <- offers_data %>% pull(faculty_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made

#Gender, race, field, position
res_demo_data <- left_join(offers_data, demographics, by = "id") %>% 
  filter(faculty_offers > 0) %>% 
  mutate(covid_offers_rescinded = 
           if_else(is.na(covid_offers_rescinded)|covid_offers_rescinded == 0, "false", "true"))

race_data <- res_demo_data %>% 
  select(id, race_ethnicity, legal_status, covid_offers_rescinded) %>% 
  mutate(race_ethnicity = str_remove(race_ethnicity, "\\(.+\\)"),
         spons_req = fct_collapse(legal_status, 
                                     "Yes" = c("Temporary student visa (e.g., F1, J1 in U.S.)",
                                                        "Temporary work visa (e.g., H1B in U.S.)",
                                                        "Applying from outside the country(ies)", 
                                                        "Choose not to disclose"),
                                     "No" = c("Citizen", "Permanent resident"))) %>% 
  separate(race_ethnicity, sep = ",", into = c("a", "b")) %>% 
  gather(a:b, key = "test", value = "race_ethnicity") %>% 
  select(-test) %>% 
  filter(!is.na(race_ethnicity))
