
#Figure 2. What has happened to the job ads?

# A. All faculty job ads----
Fig2_A <- ggplot(ten_track_summary) + 
  geom_line(aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted))+
  coord_cartesian(ylim = c(0, 1750))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "\nAll Tenure Track\nPositions", x = "\nMonth of Job Posting", color = "Year")+
  my_theme_leg_horiz

# B. Assistant Tenure Track job ads----
Fig2_B <- ggplot(ecr_summary) + 
  geom_line(aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted))+
  coord_cartesian(ylim = c(0, 1750))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "\nAssistant Professor\nTenure Track Positions", x = "\nMonth of Job Posting", color = "Year")+
  my_theme_leg_horiz

# C. Temporary positions (define)----
temp_summary <- non_track_data %>% 
  count(YearPosted, MonthPosted)

Fig2_C <- ggplot(temp_summary) + 
  geom_line(aes(x = MonthPosted, y = n, group = YearPosted, color = YearPosted))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "\nTemporary* Faculty\nPositions", x = "\nMonth of Job Posting", color = "Year",
       caption = "*Adjunct, fixed-term, and non-tenure-track lecturer or faculty")+
  my_theme_leg_horiz

# D. Region where jobs are available vs. outbreak?----
ecr_region_data <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(YearPosted == "2020" & Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(EmployerName, US_region, NumPositions) %>% 
  count(US_region, NumPositions) %>% 
  spread(NumPositions, n) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         n = `2`+`3`+`<NA>`,
         percent = get_percent(n, sum(n)),
         US_region = paste0(US_region, " (n=", n, ")")) %>% 
  select(US_region, n, percent)

Fig2_D <- ggplot(ecr_region_data, aes(x = fct_reorder(US_region, percent), y = percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  labs(y = "Assistant Professor Tenure Track\nPositions(% Posted June - December 2020)", 
       x = "\nUS Region of the\n Posting Institution")+
  my_theme_horiz

# E. University type vs job availablity ----
ecr_uni_data <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(YearPosted == "2020" & Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(EmployerName, PUI_RI, NumPositions) %>% 
  count(PUI_RI, NumPositions) %>% 
  spread(NumPositions, n) %>% 
  filter(!is.na(PUI_RI)) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         n = `2`+`3`+`<NA>`,
         percent = get_percent(n, sum(n)),
         PUI_RI = paste0(PUI_RI, " (n=", n, ")")) %>% 
  select(PUI_RI, n, percent)

Fig2_E <- ggplot(ecr_uni_data, aes(x = fct_reorder(PUI_RI, percent), y = percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  labs(y = "Assistant Professor Tenure Track Positions\n(% Posted June - December 2020)", 
       x = "\nPosting Institution\nType")+
  my_theme_horiz

#Generate Figure 2----

Fig2_DE <- plot_grid(Fig2_D, Fig2_E, labels = c('D', 'E'),
                     label_size = 18, nrow = 1)

Fig2 <- plot_grid(Fig2_A, Fig2_B, Fig2_C, Fig2_DE,
                  labels = c('A', 'B', 'C', ''),
                  label_size = 18, nrow = 4)

ggsave("Figure_2.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 7, height = 6.8)

#Relevant Fig 2 Data Calculations----
ecr_region_month_data <- ten_track_data %>% 
  filter(ECR == "Yes") %>% 
  filter(YearPosted == "2020" & Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(EmployerName, MonthPosted, US_region, NumPositions) %>% 
  count(MonthPosted, US_region, NumPositions) %>% 
  spread(NumPositions, n) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         n = `2`+`3`+`<NA>`,
         percent = get_percent(n, sum(n)),
         US_region = paste0(US_region, " (n=", n, ")")) %>% 
  select(MonthPosted, US_region, n, percent) %>% 
  filter(percent >= 2) %>% 
  arrange(desc(percent))

temp_region_month_data <- non_track_data %>% 
  filter(YearPosted == "2020" & Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(EmployerName, MonthPosted, US_region, NumPositions) %>% 
  count(MonthPosted, US_region, NumPositions) %>% 
  spread(NumPositions, n) %>% select(-`100`, -`2020`) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         `4` = 4*`4`,
         `4` = coalesce(`4`, 0),
         `6` = 6*`6`,
         `6` = coalesce(`6`, 0),
         `16` = 16*`16`,
         `16` = coalesce(`16`, 0),
         n = `2`+`3`+`4`+`6`+`16`+`<NA>`,
         percent = get_percent(n, sum(n)),
         US_region = paste0(US_region, " (n=", n, ")")) %>% 
  select(MonthPosted, US_region, n, percent) %>% 
  filter(percent >= 3) %>% 
  arrange(desc(percent))

temp_inst_data <- non_track_data %>% 
  filter(YearPosted == "2020" & Country == "USA") %>% 
  filter(MonthPosted %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  select(EmployerName, PUI_RI, NumPositions) %>% 
  count(PUI_RI, NumPositions) %>% 
  spread(NumPositions, n) %>% select(-`100`, -`2020`) %>% 
  filter(!is.na(PUI_RI)) %>% 
  mutate(`2` = 2*`2`,
         `2` = coalesce(`2`, 0),
         `3` = 3*`3`,
         `3` = coalesce(`3`, 0),
         `4` = 4*`4`,
         `4` = coalesce(`4`, 0),
         `6` = 6*`6`,
         `6` = coalesce(`6`, 0),
         `16` = 16*`16`,
         `16` = coalesce(`16`, 0),
         n = `2`+`3`+`4`+`6`+`16`+`<NA>`,
         percent = get_percent(n, sum(n))) %>% 
  select(PUI_RI, n, percent)
