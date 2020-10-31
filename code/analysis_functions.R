

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


get_plot_summary <- function(data, x, y){
  
  df <- data %>% 
    select(x, y, id) %>% 
    group_by(x, y) %>% 
    summarise(n=n()) #%>% 
    #spread(key = y, value = n) %>% 
    #mutate_all(~replace(., is.na(.), 0)) %>% 
    #mutate(percent_res = get_percent(true, false))
  
  return(df)
}
