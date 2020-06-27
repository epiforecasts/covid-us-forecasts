#' @title Get US Death Data
#' 
#' @details
#' Get & reshape JHU data
#' Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' 
#' @param data type. Can be either "daily" or "cumulative"
#' 
#' @return data.frame with Deaths Data
#'
#' @export
#' 

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
   
   saveRDS(cumulative, here::here("data", "cum_deaths_data.rds"))
   
   daily <- cumulative %>%
     # De-cumulate to daily
     dplyr::group_by(state) %>% 
     dplyr::mutate(deaths = c(0, diff(deaths)),
                   deaths = replace(deaths, deaths < 0 , 0)) %>% 
     dplyr::ungroup() 
   # Save daily deaths in all states
   saveRDS(daily, here::here("data", "deaths_data.rds"))
   
   
   if(data[1] == "cumulative"){
     return(cumulative)
   }
   if(data[1] == "daily"){  
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





#' @title Load Observed Deaths 
#' 
#' @details 
#' Loads death data from file and returns a data.table
#' 
#' @param weekly return data in a weekly format
#' @param cumulative return cumulative data instead of incidence data
#' 
#' @return data.frame with obsered deaths
#'
#' @export
#' 

load_observed_deaths <- function(weekly = FALSE, 
                                 cumulative = FALSE, 
                                 download_newest = FALSE) {
  
  # download newest data
  if (download_newest) {
    get_us_deaths()
  }
  
  
  true_deaths <- (readRDS(here::here("data", "deaths_data.rds"))) %>%
    dplyr::rename(region = state) %>%
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7))
  
  true_deaths_national <- true_deaths %>%
    dplyr::group_by(date, week) %>%
    dplyr::summarise(deaths = sum(deaths)) %>%
    dplyr::mutate(region = "US")
  
  
  true_deaths <- data.table::rbindlist(list(true_deaths, 
                                            true_deaths_national), 
                                       use.names = TRUE) %>%
    dplyr::mutate(period = "daily", 
                  data_type = "incidence")
  
  if (weekly) {
    true_deaths[, date := NULL]
    true_deaths[, `:=` (deaths = sum(deaths),
                        period = "weekly"), by = c("region", "week")]
  }
  
  if (cumulative) {
    if (weekly) {
      true_deaths[, `:=` (deaths = cumsum(deaths),
                          data_type = "cumulative"), by = c("region", "week")]
    } else {
      true_deaths[, `:=` (deaths = cumsum(deaths),
                          data_type = "cumulative"), by = c("region", "date")]
    }
    
  }
  
  return(true_deaths)
}





#' @title Get US Case Data
#' 
#' @details
#' Get & reshape JHU data
#' Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' 
#' @param data type. Can be either "daily" or "cumulative"
#' 
#' @return data.frame with Deaths Data
#'
#' @export
#' 

get_us_cases <- function(data = c("cumulative", "daily")){
  
    # Get & reshape data
      cumulative <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                             check.names = FALSE) %>%
        dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
        tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "cases") %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::group_by(Province_State, date) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::rename(state = Province_State) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))
      
      if(data[1] == "cumulative"){
        return(cumulative)
      }
      
      if(data[1] == "daily"){
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
    


library(magrittr)

get_us_deaths()
get_us_cases()
