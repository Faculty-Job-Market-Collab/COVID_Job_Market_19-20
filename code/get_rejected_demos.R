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

#12 offers rejected b/c of covid