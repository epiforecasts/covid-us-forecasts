# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  
library(magrittr)

# Arguments
# data = c("cumulative", "daily")
# anomaly_threshold = minimum death count above which to adjust anomalies (e.g 100)
# check_adjustmente = boolean, if TRUE prints which states have had data adjusted
  
# Deaths data -------------------------------------------------------------
get_german_deaths <- function(data = "daily", anomaly_threshold = 100, check_adjustment = FALSE){  

   # Get & reshape data
   cumulative <- suppressMessages(readr::read_csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Deaths_Germany.csv"))  %>%
     dplyr::rename(deaths = value, 
                   state = location_name) %>%
     dplyr::mutate(date = lubridate::ymd(date), 
                   epiweek = lubridate::epiweek(date))
     
   # Convert to daily data
   daily <- cumulative %>%
     dplyr::group_by(state) %>% 
     dplyr::mutate(deaths = c(0, diff(deaths)),
                   deaths = replace(deaths, deaths < 0 , 0)) %>%
     # Detect & adjust anomalies (>1000% change)
     dplyr::mutate(p_diff = deaths / dplyr::lag(deaths),
                   p_diff = ifelse(p_diff == "Inf", 0, p_diff),
                   extreme_diff = ifelse(abs(p_diff) > 10, TRUE, FALSE),
                   anomaly_adjusted = ifelse(extreme_diff == TRUE & deaths > anomaly_threshold, TRUE, FALSE),
                   deaths = ifelse(anomaly_adjusted == TRUE,
                                   dplyr::lag(deaths, n = 1),
                                   deaths)) %>%
     dplyr::ungroup() %>%
     dplyr::select(state, date, epiweek, deaths)
   
   
   # Re-accumulate over adjusted data
   cumulative_adj <- daily %>%
     dplyr::group_by(state) %>% 
     dplyr::mutate(deaths = cumsum(deaths))
   
# Return data
     if(data == "daily"){
     # Save daily deaths in all states
     # saveRDS(daily, here::here("data", "deaths-data-daily.rds"))
     return(daily)
   }
   
   if(data == "cumulative"){
     # saveRDS(cumulative_adj, here::here("data", "deaths-data-cumulative.rds"))
     return(cumulative_adj)
   }
   
}


# Cases data --------------------------------------------------------------

get_german_cases <- function(data = "daily"){
  
    # Get & reshape data
      case_cumulative <- read.csv("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Cases_Germany.csv",
                             check.names = FALSE) %>%
        dplyr::rename(cases = value, 
                      state = location_name) %>%
        dplyr::mutate(date = lubridate::ymd(date), 
                      epiweek = lubridate::epiweek(date))
      
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
        # saveRDS(case_daily, here::here("data", "case-data-daily.rds"))
        
        return(case_daily)
      }
}
    
  
