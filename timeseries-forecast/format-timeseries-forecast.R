# Format timeseries for ensemble/submission

# Arguments
# model_type = c("deaths-only", "deaths-on-cases")
# weekly_count = c("incident", "cumulative")

format_timeseries_forecast <- function(model_type, weekly_count){
  # Get state codes
  state_codes <- tigris::fips_codes %>%
    dplyr::select(state_code, state_name) %>%
    unique() %>%
    rbind(c("US", "US"))
  
  # Read in forecast
  daily_forecast <- readRDS(here::here("timeseries-forecast", model_type,
                                       paste0("latest-daily-", model_type, ".rds")))

# Incident counts ---------------------------------------------------------

  incident <- daily_forecast %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 7))) %>%
    group_by(week, state, quantile, model_type, date_created) %>%
    summarise(deaths = sum(deaths)) %>%
    left_join(state_codes, by = c("state" = "state_name")) %>%
    mutate(weekly_count = "incident") %>%
    select(week, state, state_code, date_created, model_type, weekly_count, quantile, deaths)

  if(weekly_count == "incident"){
    # Return
    return(incident)}
  
  else{
    
# Cumulative counts -------------------------------------------------------
      # Get cumulative data
      source(here::here("utils", "get_us_data.R"))
      
      cum_deaths_state <- get_us_deaths(data = "cumulative") %>%
        mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 7))) %>%
        mutate(sunday = ifelse(date == week, T, F)) %>%
        filter(sunday == TRUE)
      
      cum_deaths_latest <- cum_deaths_state %>%
        # add in national level
        group_by(week) %>%
        summarise(deaths = sum(deaths)) %>%
        mutate(state = "US") %>%
        bind_rows(cum_deaths_state) %>%
        # filter to latest
        group_by(state) %>%
        filter(week == max(week)) %>%
        select(state, last_week_deaths = deaths) 
      
      # Set cumulative forecasts
      cumulative <- incident %>%
        mutate(weekly_count = "cumulative") %>%
        group_by(state, quantile) %>%
        left_join(cum_deaths_latest, by = "state") %>%
        mutate(cumulative = cumsum(deaths),
               deaths = cumulative + last_week_deaths) %>%
        select(-cumulative, -last_week_deaths)
      
      # Return
      return(cumulative)
    }
}

  
