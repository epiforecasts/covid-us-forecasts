
# Packages ----------------------------------------------------------------

require(here)
require(dplyr)
require(lubridate)

## Arguments
# loc -> path to folder
# loc_name -> name in scaling data
# scal -> data frame to use for scaling see scale_cases.R for how to make this
# forecast_date -> date forecast was made
format_forecast <- function(loc = NULL, loc_name = NULL,
                            forecast_date = NULL,
                            forecast_adjustment = 4, 
                            forecast_value = "wk ahead inc death",
                            version = "1.0"){
  
  
  nowcast <- file.path(loc, "latest/nowcast.rds")
  path <- file.path(loc, "latest/time_varying_params.rds")

  nowcast <- readRDS(nowcast) %>% 
    dplyr::select(date, sample, cases, type) %>% 
    dplyr::filter(type %in% "infection_upscaled") %>% 
    dplyr::select(-type) %>% 
    dplyr::mutate(sample = as.integer(sample))
  
  forecast <- readRDS(path)
  
  ## Check to see if a forecast is present; if not return NULL
  if (is.null(forecast$raw_case_forecast)) {
    
    return(NULL)
    
  } else {
    
    forecast <- forecast$raw_case_forecast
    
    
    forecast_date <- as.Date(forecast_date)
    int_forecast_date <- dplyr::case_when(weekdays(forecast_date) == "Sunday" ~ forecast_date,
                                          TRUE ~ as.Date(lubridate::floor_date(forecast_date, unit = "week", week_start = 7)))
    
    state_codes <- tigris::fips_codes %>%
      select(state_code, state_name) %>%
      unique() %>%
      rbind(c("US", "US"))
    
    
    out <- forecast %>% 
      dplyr::mutate(sample = as.integer(sample)) %>%
      dplyr::filter(rt_type == "forecast") %>%
      dplyr::bind_rows(nowcast) %>% 
      dplyr::mutate(date = date + lubridate::days(forecast_adjustment),
                    week = floor_date(date, unit = "week", week_start = 7)) %>% 
      dplyr::filter(date >= int_forecast_date) %>%
      dplyr::group_by(sample, week) %>%
      dplyr::summarise(week_cases = sum(cases)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(week) %>%
      dplyr::group_modify( ~ {
        quantile(.x$week_cases, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
          tibble::enframe(name = "quantile", value = "value") %>%
          dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
      }) %>%
      dplyr::mutate(forecast_date = forecast_date,
                    target_end_date = week + 6,
                    location = state_codes$state_code[state_codes$state_name == loc_name],
                    type = "quantile",
                    target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))), forecast_value, sep = " ")) %>%
      ungroup() %>%
      select(forecast_date, target, target_end_date, location, type, quantile, value)
    
    out <- out %>%
      dplyr::bind_rows(out %>%
                         filter(quantile == 0.5) %>%
                         mutate(type = "point") %>%
                         select(-quantile))
    
    return(out)
  }
  
}
