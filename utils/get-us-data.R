# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  
library(magrittr)
# Arguments
# data = c("cumulative", "daily")
  
# Deaths data -------------------------------------------------------------
get_us_deaths <- function(data = "daily", anomaly_threshold = 100){  

   # Get & reshape data
   cumulative <- suppressMessages(
     readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")) %>% 
      dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
      tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
      dplyr::mutate(date = lubridate::mdy(date)) %>%
      dplyr::group_by(Province_State, date) %>%
      dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
      dplyr::rename(state = Province_State) %>%
      dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
   
     daily <- cumulative %>%
     # De-cumulate to daily
       dplyr::group_by(state) %>% 
       dplyr::mutate(deaths = c(0, diff(deaths)),
                     deaths = replace(deaths, deaths < 0 , 0),
                     p_diff = deaths / dplyr::lag(deaths),
                     p_diff = ifelse(p_diff == "Inf", 0, p_diff),
                     extreme_diff = ifelse(p_diff > 10, TRUE, FALSE),
                     adjusted = ifelse(extreme_diff == TRUE & deaths > anomaly_threshold, TRUE, FALSE),
                     raw_deaths = deaths,
                     deaths = ifelse(adjusted == TRUE,
                                      dplyr::lag(deaths),
                                      deaths)) %>%
       dplyr::ungroup() %>%
      dplyr::select(-extreme_diff, -p_diff)
     
     # Re-accumulate over adjusted data
     cumulative_adj <- daily %>%
       dplyr::group_by(state) %>% 
       dplyr::mutate(deaths = cumsum(deaths),
                     raw_deaths = cumsum(raw_deaths)
                     )
     
# Return data
     if(data == "daily"){
     # Save daily deaths in all states
     saveRDS(daily, here::here("data", "deaths-data-daily.rds"))
     return(daily)
   }
   
   if(data == "cumulative"){
     saveRDS(cumulative_adj, here::here("data", "deaths-data-cumulative.rds"))
     return(cumulative_adj)
   }
   
}


# Cases data --------------------------------------------------------------

get_us_cases <- function(data = "daily"){
  
    # Get & reshape data
      case_cumulative <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                             check.names = FALSE) %>%
        dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
        tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "cases") %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::group_by(Province_State, date) %>%
        dplyr::summarise(cases = sum(cases), .groups = "drop_last") %>%
        dplyr::rename(state = Province_State) %>%
        dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
      
      if(data == "cumulative"){
        saveRDS(case_cumulative, here::here("data", "case-data-cumulative.rds"))
        return(case_cumulative)
      }
      
      if(data == "daily"){
        case_daily <- case_cumulative %>%
          # De-cumulate to daily
          dplyr::group_by(state) %>% 
          dplyr::mutate(cases = c(0, diff(cases)),
                        cases = replace(cases, cases < 0 , 0)) %>% 
          dplyr::ungroup() 
        # Save daily cases in all states
        saveRDS(case_daily, here::here("data", "case-data-daily.rds"))
        
        return(case_daily)
      }
}
    
  
