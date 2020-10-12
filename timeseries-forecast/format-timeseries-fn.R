# Format timeseries in output ready for submission (or ensembling)

# Arguments with examples
# model_type = c("deaths-only", "deaths-on-cases")
# right_truncate_weeks <- 1 [as used in forecasting: to prevent over-adding cumulative data]

library(dplyr)

format_timeseries <- function(model_type, forecast_date, submission_date, 
                                     right_truncate_weeks, quantiles_out){
   
# Set up -------------------------------------------------------------

# Get state codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(location = state_code, state = state_name) %>%
  unique() %>%
  rbind(c("US", "US")) %>%
  dplyr::mutate(state = ifelse(state == "U.S. Virgin Islands", "Virgin Islands", state))
 
# Read in forecast
samples <- readRDS(here::here("timeseries-forecast", model_type, "raw-samples", "dated", 
                                     paste0(forecast_date, "-samples-weekly-", model_type, ".rds")))

# Get quantiles
raw_weekly_forecast <- samples %>%
  dplyr::group_by(state, epiweek_target) %>%
  dplyr::group_modify( ~ as.data.frame(quantile(.x$deaths, probs = quantiles_out, na.rm = T))) %>%
  dplyr::mutate(quantile = quantiles_out,
         date_created = unique(samples$date_created),
         model_type = unique(samples$model_type)) %>%
  dplyr::rename(deaths = 3, epiweek = epiweek_target) %>%
  dplyr::ungroup()

# Set epiweek to target date conversion
forecast_date <- unique(raw_weekly_forecast$date_created)
submission_date_epiweek <- lubridate::epiweek(submission_date)

# Incident forecast -------------------------------------------

incident_forecast <- raw_weekly_forecast %>%
  # Filter to current and future epiweeks
  dplyr::filter(epiweek >= submission_date_epiweek) %>%
  # Format for submission
  dplyr::left_join(state_codes, by = c("state")) %>%
  epiweek_to_date() %>%
  dplyr::mutate(submission_date = submission_date,
                type = "quantile",
                target = paste(epiweek - submission_date_epiweek + 1,
                               "wk ahead inc death",
                               sep = " ")) %>%
  dplyr::select(forecast_date = date_created, submission_date, target, 
                target_end_date = epiweek_end_date, 
                type, quantile, value = deaths, location = state)
 
# Add point forecast
incident_forecast <- incident_forecast %>%
  dplyr::bind_rows(incident_forecast %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point") %>%
                     dplyr::select(-quantile)) %>%
  dplyr::mutate(value = floor(value))


# Cumulative formatting ----------------------------------------------------
  cumulative_data <- get_us_deaths(data = "cumulative")
  
  cumulative_deaths <- cumulative_data %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == min(max(date), forecast_date)) %>%
    dplyr::add_row(state="US", deaths = sum(.$deaths), date = forecast_date) %>%
    dplyr::left_join(state_codes, by = "state")
  
  cumulative_forecast <- incident_forecast %>%
    dplyr::left_join(dplyr::select(cumulative_deaths, location, deaths),
                     by = "location") %>%
    dplyr::group_by(location, quantile, type) %>%
    dplyr::mutate(value = cumsum(value),
                  value = value + deaths,
                  target = stringr::str_replace_all(target, "inc", "cum")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-deaths)


# Bind cumulative and incident --------------------------------------------

out <- dplyr::bind_rows(incident_forecast, cumulative_forecast)

return(out)
}



