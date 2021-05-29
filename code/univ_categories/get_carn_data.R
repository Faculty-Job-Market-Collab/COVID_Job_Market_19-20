#get list of states & regions
us_regions <- read_csv("data/us_region_list.csv")

AFR <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Ivory Coast", "Democratic Republic of the Congo", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Eswatini", "Togo", "Uganda", "Tanzania", "Zambia", "Zimbabwe")

AMR <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela")

SEAR <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", "Maldives", "Myanmar", "Nepal", "Sri Lanka", "Thailand", "Timor-Leste")

EUR <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan")

EMR <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Pakistan", "Palestine", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen")

WPR <- c("Australia", "Brunei", "Cambodia", "China", "Cook Islands", "Fiji", "Japan", "Kiribati", "Laos", "Malaysia", "Marshall Islands", "Micronesia", "Mongolia", "Nauru", "New Zealand", "Niue", "Palau", "Papua New Guinea", "Philippines", "Samoa", "Singapore", "Solomon Islands", "South Korea", "Taiwan", "Tonga", "Tuvalu", "Vanuatu", "Vietnam")

get_world_region <- function(x){
  case_when(
    x %in% AFR ~ "African",
    x %in% AMR ~ "The Americas",
    x %in% SEAR ~ "South-East Asian",
    x %in% EUR ~ "European",
    x %in% EMR ~ "Eastern Mediterranean",
    x %in% WPR ~ "Western Pacific",
    x == "Canada" | x == "canada" ~ "Canada",
    x == "USA" ~ "USA"
  )
}

#read in cleaned carnegie data----
clean_carn_data <- read_csv("data/clean_carnegie_data.csv") %>% 
  select(NAME, STABBR, DOCRSDEG, HBCU, HSI, MSI, `S&ER&D`, 
         SOCSC_RSD, STEM_RSD, LANDGRNT,	MEDICAL,	TRIBAL, WOMENS)

#determine r1 vs pui status----
pui_carn <- clean_carn_data %>% 
  mutate(PUI_RI = if_else(DOCRSDEG >= 21, "RI", "PUI"))

#merge region to carn data ----
reg_carn <- left_join(pui_carn, us_regions, 
                      by = c("STABBR" = "State_abbvr")) %>% 
  distinct()

#source non-carn universities/institutions
non_carn_unis <- read_csv("data/non-carnegie_unis.csv") %>% ## Need to work on matching inst names for non-carn inst
  mutate(Institution = str_to_title(Institution),
         Institution = str_remove_all(Institution, "'"),
         `Full Institution Name` = str_remove_all(`Full Institution Name`, "'"),
         `Full Institution Name` = str_to_title(`Full Institution Name`))

#get nonUS/nonCarn inst
fix_non_carn <- function(x){
  case_when(
    str_detect(x, "Calagary") ~ "University Of Calgary",
    str_detect(x, "Curie") ~ "Pierre And Marie Curie University",
    str_detect(x, "Zürich|Zurich") ~ "University Of Zurich",
    str_detect(x, "Qub") ~ "Queens University Belfast",
    TRUE ~ x
  )
}
