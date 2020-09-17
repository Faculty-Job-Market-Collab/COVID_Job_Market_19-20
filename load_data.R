library(tidyverse)

raw_data <- read_csv("../data/raw_job_survey_data")

q_list <- raw_data[1,] %>% gather(., key = Q_number, value = Question)

clean_data <- raw_data[-c(1,2,3),] %>% select(-(1:17)) 

clean_data <- mutate(clean_data, id = rownames(clean_data))

demographics <- clean_data %>% select(Q6:Q23)

covid <- clean_data %>% select(Q114, Q115, Q116)
