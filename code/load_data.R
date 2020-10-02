library(tidyverse) #for data wrangling
library(data.table) #for setnames()

raw_data <- read_csv("../data/raw_job_survey_data.csv") #load data

q_list <- read_csv("../data/question_legend.csv") #csv of q numbers, full q, & column names

q_num <- q_list %>% pull(Q_number)#list of q numbers

q_data <- q_list %>% pull(Data) #list of column names

data <- raw_data[-c(1,2,3),] %>% #drop non-data rows
  filter(as.numeric(Progress) >= 33) %>% #drop entries w/ less than 33% completion
  select(-(1:17)) #drop unnecessary data columns

setnames(data, old = q_num, new = q_data) #rename columns

clean_data <- mutate(data, id = rownames(data)) %>% #generate unique ids
  filter(previous_tenure_track == "No" | is.na(previous_tenure_track)) %>%  #drop responders reporting a previous tenure track postion
  select(-previous_tenure_track)
## question-based datasets----

demographics <- select(clean_data, position:biomedical, id)

covid_only <- select(clean_data, contains("covid"), id)

qualifications <- select(clean_data, 'peer-reviewed_papers':teaching_types, id)

app_outcomes <- select(clean_data, apps_submitted:application_cycles, id)

network <- select(clean_data, advisor_rank:scholar_hindex_2015_2, id, -contains("research_min"))

preparation <- select(clean_data, id, contains("research_min"), app_feedback:workshop_data)

perceptions <- select(clean_data, id, covid_alter_research:commitment_impact)

free_resp <- select(clean_data, id, comments)
