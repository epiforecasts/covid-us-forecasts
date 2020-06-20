# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  
library(magrittr)
  
# Deaths data -------------------------------------------------------------
get_us_deaths <- function(data = c("cumulative", "daily")){  

   # Get & reshape data
   cumulative <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
      dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
      tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
      dplyr::mutate(date = lubridate::mdy(date)) %>%
      dplyr::group_by(Province_State, date) %>%
      dplyr::summarise(deaths = sum(deaths)) %>%
      dplyr::rename(state = Province_State) %>%
      dplyr::arrange(date) %>%
     dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
   
   if(data == "cumulative"){
     return(cumulative)
   }
   
   if(data == "daily"){
     daily <- cumulative %>%
     # De-cumulate to daily
       dplyr::group_by(state) %>% 
       dplyr::mutate(deaths = c(0, diff(deaths)),
                     deaths = replace(deaths, deaths < 0 , 0)) %>% 
       dplyr::ungroup() 
   # Save daily deaths in all states
    saveRDS(daily, here::here("data", "deaths_data.rds"))
    
    return(daily)
   }
}

   
# Argument for last_week_min
# 
# if(!is.null(last_week_min)){
#   # Filter to states meeting minimum threshold
#   state_count <- daily %>%
#     dplyr::mutate(week = date >= (max(date)-7)) %>%
#     dplyr::group_by(state, week) %>%
#     dplyr::summarise(week_count = sum(deaths)) %>%
#     dplyr::filter(week == TRUE & week_count >= last_week_min)
#   state_deaths <- dplyr::filter(daily, state %in% state_count$state)  
# Cases data --------------------------------------------------------------

get_us_cases <- function(data = c("cumulative", "daily")){
  
    # Get & reshape data
      cumulative <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
        dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
        tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "cases") %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::group_by(Province_State, date) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::rename(state = Province_State) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
      
      if(data == "cumulative"){
        return(cumulative)
      }
      
      if(data == "daily"){
        daily <- cumulative %>%
          # De-cumulate to daily
          dplyr::group_by(state) %>% 
          dplyr::mutate(cases = c(0, diff(cases)),
                        cases = replace(cases, cases < 0 , 0)) %>% 
          dplyr::ungroup() 
        # Save daily cases in all states
        saveRDS(daily, here::here("data", "cases_data.rds"))
        
        return(daily)
      }
}
    
  
