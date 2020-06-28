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

## Here again there is an odd mix of tidyverse and data.table
## See note on epiweek
load_observed_deaths <- function(weekly = FALSE, 
                                 cumulative = FALSE, 
                                 download_newest = FALSE) {
  
  # download newest data
  if (download_newest) {
    get_us_deaths()
  }
  
  
  true_deaths <- (readRDS(here::here("data", "deaths_data.rds"))) %>%
    dplyr::rename(region = state) %>%
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7))  %>% 
    dplyr::mutate(epiweek = lubridate::epiweek(date),
                  day = ordered(weekdays(as.Date(date)),
                                levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                  epiweek = as.numeric(paste0(epiweek, ".", as.numeric(day))))
  
  true_deaths_national <- true_deaths %>%
    dplyr::group_by(date, epiweek, day, week) %>%
    dplyr::summarise(deaths = sum(deaths)) %>%
    dplyr::mutate(region = "US")
  
  ## switch to dplyr
  true_deaths <- data.table::rbindlist(list(true_deaths, 
                                            true_deaths_national), 
                                       use.names = TRUE) %>%
    dplyr::mutate(period = "daily", 
                  data_type = "incidence")
  ## dplyr
  if (weekly) {
    true_deaths[, date := NULL]
    true_deaths[, day := NULL]
    true_deaths[, `:=` (deaths = sum(deaths),
                        period = "weekly"), by = c("region", "epiweek", "data_type")]
    true_deaths <- unique(true_deaths)
  }
  
  
  
  if (cumulative) {
    if (weekly) {
      true_deaths <- true_deaths[,  .(deaths = cumsum(deaths),
                                      data_type = "cumulative", 
                                      period = period), by = c("region", "epiweek", "week")]
    } else {
      true_deaths[, `:=` (deaths = cumsum(deaths),
                          data_type = "cumulative"), by = c("region", "date")]
    }
    
  }
  
  
  
  # merge with state codes 
  state_codes <- tigris::fips_codes %>%
    dplyr::select(state_code, state_name) %>%
    unique() %>%
    rbind(c("US", "US")) %>%
    dplyr::rename(region = state_name)
  
  
  true_deaths <- true_deaths %>%
    dplyr::left_join(state_codes)
  
  return(true_deaths)
}

