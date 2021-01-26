# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  
library(magrittr)

# Arguments
# data = c("cumulative", "daily")
# anomaly_threshold = minimum death count above which to adjust anomalies (e.g 100)
# check_adjustmente = boolean, if TRUE prints which states have had data adjusted
  
# Deaths data -------------------------------------------------------------
get_us_deaths <- function(data = "daily", anomaly_threshold = 100, check_adjustment = FALSE){  

   # Get & reshape data
   cumulative <- suppressMessages(readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")) %>% 
      dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
      tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
      dplyr::mutate(date = lubridate::mdy(date)) %>%
      dplyr::group_by(Province_State, date) %>%
      dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
      dplyr::rename(state = Province_State) %>%
      dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))

   # Convert to daily data
   daily <- cumulative %>%
     dplyr::group_by(state) %>% 
     dplyr::mutate(deaths = c(0, diff(deaths)),
                   deaths = replace(deaths, deaths < 0 , 0)) %>%
     dplyr::ungroup()
     
# Return data
     if(data == "daily"){
     return(daily)
   }
   
   if(data == "cumulative"){
     return(cumulative)
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
        return(case_cumulative)
      }
      
      if(data == "daily"){
        case_daily <- case_cumulative %>%
          # De-cumulate to daily
          dplyr::group_by(state) %>% 
          dplyr::mutate(cases = c(0, diff(cases)),
                        cases = replace(cases, cases < 0 , 0)) %>% 
          dplyr::ungroup() 
        
        return(case_daily)
      }
}
    
  
