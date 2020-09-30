library(tidyverse) #for data wrangling
library(data.table) #for setnames()

raw_data <- read_csv("data/raw_job_survey_data.csv") #load data

q_list <- read_csv("data/question_legend.csv") #csv of q numbers, full q, & column names

q_num <- q_list %>% pull(Q_number)#list of q numbers

q_data <- q_list %>% pull(Data) #list of column names

data <- raw_data[-c(1,2,3),] %>% select(-(1:17)) #drop unnecessary data 

setnames(data, old = q_num, new = q_data) #rename columns

data <- mutate(data, id = rownames(data)) #generate unique ids

## question-based datasets----

demographics <- data %>% select(position:biomedical, id)

covid_only <- data %>% select(contains("covid"), id)

