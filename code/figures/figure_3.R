# Figure 3. Alternate strategies for the job search

#setup for A &B ----
#source("code/get_strategy_data.R")

# A. March (full survey) change in strategy by research field----
Fig_3A <- get_plot_summary(strategy_demo_data, "research_category", "covid_alter") %>% 
  filter(str_detect(research_category, "Integrated Sciences|NA") == FALSE) %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=percent_res))+
  coord_flip()+
  scale_y_continuous(limits = c(0,30), expand = c(0,0))+
  labs(y = "Early Pandemic Respondents That\nAltered Their Research Statement\nto Focus on Remote or\nComputational Research (%)", x = "\nResearch Category")+
  my_theme_horiz

# B. March change in strategy by 1st gen PhD----
Fig_3B <- get_plot_summary(strategy_demo_data, "first_gen_phd", "covid_alter") %>% 
  filter(str_detect(first_gen_phd, "n=0") == FALSE) %>% 
  ggplot()+
  geom_col(aes(x = first_gen_phd, y=percent_res))+
  coord_flip()+
  scale_y_continuous(limits = c(0,30), expand = c(0,0))+
  labs(y = "Early Pandemic Respondents That\nAltered Their Research Statement\nto Focus on Remote or\nComputational Research (%)", x = "\nFirst Generation PhD Status\n")+
  my_theme_horiz

#Setup for C & D----
#source("code/get_search_strategy_results.R")

alt_list <- c("Somewhat agree", "Strongly agree", "Agree")

c_d_data <- strategy_data %>% 
  filter(current_app == "Yes") %>% 
  filter(str_detect(concern, "altered") == TRUE) %>% 
  select(-current_app, -reasons) %>% 
  mutate(alt_strategy = if_else(extent %in% alt_list, "yes", "no"),
         concern = fct_recode(concern,
                              "Now have pandemic-related areas" = "altered_research_covid",
                              "Be more 'remote-friendly'" = "altered_research_remote",
                              "More online teaching practices" = "altered_teaching_online"))

#n_res <- nrow(c_d_data)

# C. Fall alternate research &/or teaching strategies by research category----
Fig_3c <- c_d_data %>% 
  filter(concern != "More online teaching practices") %>% 
  select(id, research_category, concern, alt_strategy) %>% 
  distinct() %>% 
  count(research_category, concern, alt_strategy) %>% 
  spread(alt_strategy, n) %>% 
  mutate(no = coalesce(no, 0),
         yes = coalesce(yes, 0),
         per_yes = get_percent(yes, (yes+no)),
         research_category = paste0(research_category, " (n=", no+yes, ")")) %>% 
  ggplot()+
  geom_col(aes(x = research_category, y=per_yes))+
  coord_flip()+
  facet_wrap(~concern)+
  scale_y_continuous(limits = c(0,100), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents That\nAltered Their Research Statement (%)", x = "\nResearch Category")+
  my_theme_horiz+
  theme(panel.spacing = unit(2, "lines"))

# D. Fall alt research &/or teaching strategies according to institution type (PUI vs RI)----
Fig_3d <- c_d_data %>% 
  select(id, desired_institution, concern, alt_strategy) %>% 
  distinct() %>% 
  count(desired_institution, concern, alt_strategy) %>% 
  spread(alt_strategy, n) %>% 
  mutate(no = coalesce(no, 0),
         yes = coalesce(yes, 0),
         per_yes = get_percent(yes, (yes+no)),
         desired_institution = fct_recode(desired_institution,
                                          "Research\nIntensive" = "Research Intensive",
                                          "Primarily Undergraduate\nServing" = "Primarily Undergraduate Serving"),
         desired_institution = paste0(desired_institution, " (n=", no+yes, ")")) %>%
  ggplot()+
  geom_col(aes(x = desired_institution, y=per_yes))+
  coord_flip()+
  facet_wrap(~concern)+
  scale_y_continuous(limits = c(0,50), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents That Altered\nTheir Research or Teaching Statements (%)", x = "\nDesired Institution Type\n")+
  my_theme_horiz+
  theme(panel.spacing = unit(1, "lines"))

# E. Commitment to academic career in Mar vs % in fall----
e_data_fall <- strategy_data %>% select(id, current_app) %>% distinct() %>% 
  #filter(!is.na(current_app)) %>% 
  count(current_app) %>% 
  mutate(percent= get_percent(n, sum(n)),
         current_app = replace_na(current_app, "No response"),
         current_app = fct_recode(current_app,
                                  "No, I decided to look\noutside of academia" = "No, I decided to change career paths and look outside of academia",
                                  "No, I decided to wait" = "No, I decided to wait to a later time"),
         current_app = paste0(current_app , " n(=", n, ")"))

e_data_spring <- app_outcomes %>% 
  select(id, faculty_offers) %>% distinct() %>% 
  right_join(., perceptions, by = "id") %>%
  filter(faculty_offers == 0 | is.na(faculty_offers)) %>% 
  select(id, faculty_offers, commitment_impact) %>% distinct() %>% 
  filter(!is.na(commitment_impact)) %>% 
  count(commitment_impact) %>% 
  mutate(percent = get_percent(n, sum(n)),
         commitment_impact = replace_na(commitment_impact, "No response"),
         commitment_impact = paste0(commitment_impact, " n(=", n, ")")) 

spring_2020 <- e_data_spring %>% 
  ggplot()+
  geom_col(aes(x = commitment_impact, y = percent))+
  coord_flip()+
  scale_y_continuous(limits = c(0,50), expand = c(0,0))+
  labs(y = "Early Pandemic Respondents (%)*\n(n=304)", 
       x = "\nCommitment to\nAttaining a Faculty Position\n",
       caption = "*who did not recieve a faculty offer")+
  my_theme_horiz

fall_2020 <- e_data_fall %>% 
  ggplot()+
  geom_col(aes(x = current_app, y = percent))+
  coord_flip()+
  scale_y_continuous(limits = c(0,75), expand = c(0,0))+
  labs(y = "Mid-Pandemic Respondents (%)\n(n=78)", 
       x = "\nResponse to Submitting\nFaculty Position Applications\n")+
  my_theme_horiz

#generate Fig 3-----

Fig_3ab <- plot_grid(Fig_3A, Fig_3c, labels = c('A', 'B'),
                     label_size = 18, nrow = 1, rel_widths = c(1, 1.25))

Fig_3cd <- plot_grid(Fig_3B, Fig_3d, labels = c('C', 'D'),
                     label_size = 18, nrow = 1, rel_widths = c(.6, 1))

Fig_3ef <- plot_grid(spring_2020, fall_2020,
                     labels = c('E', 'F'), label_size = 18)

Fig_3 <- plot_grid(Fig_3ab, Fig_3cd, Fig_3ef,
                   nrow = 3)

#save Fig 3
ggsave("Figure_3.png", device = 'png', units = "in", scale = 1.75,
       path = 'figures', width = 12, height = 8)
