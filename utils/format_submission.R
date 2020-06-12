
# Packages ----------------------------------------------------------------

require(here)
require(dplyr)
require(lubridate)

## Arguments
# loc: path to folder
# loc_name: name of region
# forecast_date: date forecast is made
# forecast_adjustment: 
format_forecast <- function(loc = NULL, loc_name = NULL,
                            forecast_date = NULL,
                            forecast_adjustment = 4, 
                            version = "1.0"){
  
  ## Get cumulative case counts for the end of the previous week
  deaths_data <- readRDS(here::here("rt-forecast/data/deaths_data.rds")) %>%
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7)) %>%
    dplyr::group_by(state, week) %>%
    dplyr::summarise(week_deaths = sum(deaths))
  cumulative_deaths_data <- deaths_data %>%
    bind_rows(deaths_data %>%
                dplyr::group_by(week) %>%
                dplyr::summarise(week_deaths = sum(week_deaths)) %>%
                dplyr::mutate(state = "US")) %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(cum_week_deaths = cumsum(week_deaths)) %>%
    dplyr::select(-week_deaths) %>%
    dplyr::filter(week == floor_date(forecast_date, unit = "week", week_start = 7)-7,
                  state == loc_name) %>%
    .$cum_week_deaths
  
  
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
    
    
    inc <- forecast %>%
      dplyr::filter(rt_type == "forecast") %>%
      dplyr::mutate(sample = as.integer(sample)) %>%
      # filter by samples temporary fix until this is addressed in EpiNow/estimate_R0.R
      dplyr::group_by(sample, date) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      #
      dplyr::bind_rows(nowcast) %>%
      dplyr::mutate(date = date + lubridate::days(forecast_adjustment),
                    week = floor_date(date, unit = "week", week_start = 7)) %>% 
      dplyr::filter(date >= int_forecast_date) %>%
      dplyr::group_by(sample, week) %>%
      dplyr::summarise(week_cases = sum(cases)) %>%
      dplyr::ungroup()
    
    cum <- inc %>%
      group_by(sample) %>%
      mutate(week_cases = cumsum(week_cases) + cumulative_deaths_data)
    
    process_data = function(df, name){
      
      df <- df %>%
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
                      target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))), "wk ahead", name, "death", sep = " ")) %>%
        ungroup() %>%
        select(forecast_date, target, target_end_date, location, type, quantile, value)
      
      df <- df %>%
        dplyr::bind_rows(df %>%
                           filter(quantile == 0.5) %>%
                           mutate(type = "point") %>%
                           select(-quantile))
      
      return(df)
      
    }
    
    
    out <- process_data(inc, "inc") %>%
      bind_rows(process_data(cum, "cum"))
    
    
    return(out)
  }
  
}
