#what happens to adjunct positions?

non_track_data <- clean_herc_data %>% 
  filter(TenureTrack == "No") %>% 
  filter(str_detect(Title, "Postdoc|Post-Doc") == FALSE) %>% 
  filter(str_detect(Title, "Lectur|Adjunct|Prof|Facul|Instructor") == TRUE) %>% 
  mutate(YearPosted = year(DateTimePosted) %>% as_factor(.),
         MonthPosted = month(DateTimePosted, label = TRUE),
         MonthPosted = factor(MonthPosted, levels = month_levels),
         Adjunct = if_else(str_detect(Title, "Adjunct|Lecturer"), "Yes", "No"),
         NonTrack = if_else(str_detect(Description, "non tenure|fixed term"), "Yes", "No")) 

#All adjunct positions
adjunct_summary <- non_track_data %>% 
  filter(Adjunct == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


#All non-tenure track faculty positions
fixed_summary <- non_track_data %>% 
  filter(str_detect(Title, "Assistant|Prof") == TRUE) %>% 
  filter(NonTrack == "Yes") %>% 
  group_by(YearPosted, MonthPosted) %>% 
  summarise(n = n())


