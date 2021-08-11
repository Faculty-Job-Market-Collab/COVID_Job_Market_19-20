
# Central Question here is: Was there a decrease in positions that closed between January 2020 and March 2021 
# compared prior year trends? 

# We need to extract the relevant data first.
# We want tenure track faculty positions specifically for early career applicants

# Task 1: Identify all entries that are for tenure track faculty positions for early career applicants. 
ten_track_data <- herc_data %>% 
  filter(TenureTrack == "Yes") %>% 
  filter(str_detect(Description, "fixed term") == FALSE) %>% 
  filter(str_detect(Title, "Postdoc") == FALSE) %>% 
  mutate(ECR = if_else(str_detect(Title, "Assistant"), "Yes", "No")) 

#All tenure track positions
ten_track_summary <- ten_track_data %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


#All Assistant Professor Tenure Track positions
ecr_summary <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


