# Format timeseries for ensemble/submission

# Arguments
# model_type = c("deaths-only", "deaths-on-cases")
# weekly_count = c("incident", "cumulative")

format_timeseries_forecast <- function(model_type){
  # Get state codes
  state_codes <- tigris::fips_codes %>%
    dplyr::select(state_code, state_name) %>%
    unique() %>%
    rbind(c("US", "US"))
  
  # Read in forecast
  # quick hack: changed name to weekly - needs update in the future
  daily_forecast <- readRDS(here::here("timeseries-forecast", model_type,
                                       paste0("latest-weekly-", model_type, ".rds")))

# Incident counts ---------------------------------------------------------

  incident <- daily_forecast %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 7))) %>%
    group_by(week, state, quantile, model_type, date_created) %>%
    summarise(deaths = sum(deaths)) %>%
    # a bit unclear why ungrouping isn't done by summarise?
    ungroup() %>%
    # somewhere one state code is NA??
    left_join(state_codes, by = c("state" = "state_name")) %>%
    mutate(weekly_count = "incident") %>%
    dplyr::mutate(forecast_date = date_created, 
                  target_end_date = week + 6, 
                  target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))),
                        "wk ahead",
                        "inc",
                        "death",
                        sep = " "), 
                  type = "quantile") %>%
    dplyr::rename(location = state_code, 
                  value = deaths) %>%
    select(forecast_date, target, target_end_date, location, type, quantile, value, state) 
  
  # add point value to forecast
  incident <- rbind(incident, 
                    incident %>%
                      dplyr::filter(quantile == 0.5) %>%
                      dplyr::mutate(type = "point", 
                                    quantile = NA))

  
# Cumulative counts -------------------------------------------------------
      # Get cumulative data
  source(here::here("utils", "get-us-data.R"))
  
  cum_deaths_state <- get_us_deaths(data = "cumulative") %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6))) %>%
    mutate(saturday = ifelse(date == week, T, F)) %>%
    filter(saturday == TRUE)
  
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
    mutate(target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))),
                          "wk ahead",
                          "cum",
                          "death",
                          sep = " ")) %>%
    group_by(state, quantile) %>%
    left_join(cum_deaths_latest, by = "state") %>%
    mutate(value = cumsum(value),
           value = value + last_week_deaths) %>%
    select(-last_week_deaths)
  
  # combine
  full <- rbind(incident, cumulative) 
  full$state = NULL
  
  # Return
  return(full)
}



deaths_on_cases <- format_timeseries_forecast(model_type = "deaths-on-cases")
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases", 
                                             "submission-files",
                                             "latest-weekly-deaths-on-cases.csv"))

deaths_only <- format_timeseries_forecast(model_type = "deaths-only")
readr::write_csv(deaths_only, here::here("timeseries-forecast", "deaths-only", 
                                         "submission-files",
                                         "latest-weekly-deaths-only.csv"))



  
