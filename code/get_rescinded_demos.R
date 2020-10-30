#explore demographics of offers rescinded due to covid
#requires data from "get_offers_data.R"

offers_made <- offers_data %>% pull(faculty_offers) %>% sum(., na.rm = TRUE)

offers_rescinded <- offers_data %>% pull(covid_offers_rescinded) %>% sum(., na.rm = TRUE)

percent_rescinded <- (offers_rescinded/offers_made)*100 #need to correct for differing interpretations, some did not include rescinded offers with the offers made
