

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 1)
  return(percent)
}

`%not_in%` <- Negate(`%in%`)

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


get_plot_summary <- function(data, x, y){ #x and y must be provided in quotations
  
  df <- data %>% 
    select(!!sym(x), !!sym(y), id) %>% #!!sym() allows tidyverse functions to evaluate x and y as column names
    group_by(!!sym(x), !!sym(y)) %>% 
    summarise(n=n()) %>% 
    spread(key = !!sym(y), value = n) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% #replace na values w/ 0 to allow percent calculations
    mutate(percent_res = get_percent(true, false),
           n = true+false,
           r = paste0("r=", true)) %>% 
    mutate('{x}' := paste0(!!sym(x), " (n=", n, ")"))#add n of each group to the group name; '{}' := allows mutate to evaluate the variable x as a column
  
  return(df)
}

