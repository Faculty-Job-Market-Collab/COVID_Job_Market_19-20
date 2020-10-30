library(tidyverse)
library(lubridate)

# Central Question here is: Was there a decrease in positions that closed between January 2020 and March 2021 
# compared prior year trends? 

# We need to extract the relevant data first.
# We want tenure track faculty positions specifically for early career applicants

# Task 1: Identify all entries that are for tenure track faculty positions for early career applicants. 
raw_job_data <- read_csv("data/raw_faculty_herc_data.csv")