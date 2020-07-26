# Format timeseries in output ready for submission (or ensembling)

# Arguments with examples
# model_type = c("deaths-only", "deaths-on-cases")
# right_truncate_weeks <- 1 [as used in forecasting: to prevent over-adding cumulative data]

format_timeseries <- function(model_type, right_truncate_weeks, quantiles_out){
  
# Raw data set up -------------------------------------------------------------

# Get state codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US")) %>%
  mutate(state_name = ifelse(state_name == "U.S. Virgin Islands", "Virgin Islands", state_name))

# Read in forecast
samples <- readRDS(here::here("timeseries-forecast", model_type, "raw-samples",
                                     paste0("latest-samples-weekly-", model_type, ".rds")))

# Get quantiles
raw_weekly_forecast <- samples %>%
  group_by(state, epiweek_target) %>%
  group_modify( ~ as.data.frame(quantile(.x$deaths, probs = quantiles_out, na.rm = T))) %>%
  mutate(quantile = quantiles_out,
         date_created = unique(samples$date_created),
         model_type = unique(samples$model_type)) %>%
  rename(deaths = 3, epiweek = epiweek_target) %>%
  ungroup()

# Set epiweek to target date conversion
forecast_date <- unique(raw_weekly_forecast$date_created)
forecast_date_epiweek <- lubridate::epiweek(forecast_date)

# Cumulative formatting ----------------------------------------------------
  
# --- Get cumulative weekly data ---
cumulative_data <- get_us_deaths(data = "cumulative")

# State cumulative data
cumulative_deaths_state <- cumulative_data %>%
  dates_to_epiweek() %>%
  dplyr::filter(epiweek_end == TRUE)

# National cumulative data
cumulative_deaths_national <- cumulative_deaths_state %>%
  dplyr::group_by(epiweek) %>%
  dplyr::summarise(deaths = sum(deaths),
                   state = "US",
                   .groups = "drop_last") %>%
  dplyr::ungroup()

# Bind
cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)

# Filter to last week of data shown to model
last_week_cumulative_deaths <- cumulative_deaths %>%
  dplyr::filter(epiweek == max(epiweek)-right_truncate_weeks) %>%
  dplyr::select(-epiweek, -date)

# --- Create cumulative forecast ---
raw_cumulative_forecast <- raw_weekly_forecast %>%
  dplyr::group_by(quantile, state) %>%
  dplyr::mutate(deaths = cumsum(deaths)) %>%
  ungroup()
  
# Join historical cumulative deaths to forecasts  
cumulative_forecast <- raw_cumulative_forecast %>%  
  dplyr::left_join(last_week_cumulative_deaths, by = "state") %>%
  dplyr::mutate(deaths = deaths.x + deaths.y) %>%
  # Filter to current and future epiweeks
  dplyr::filter(epiweek >= forecast_date_epiweek) %>%
  # Format for submission
  dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
  epiweek_to_date() %>%
  dplyr::mutate(type = "quantile",
                target = paste(epiweek - forecast_date_epiweek + 1,
                               "wk ahead cum death",
                               sep = " ")) %>%
  dplyr::select(forecast_date = date_created, target, target_end_date = epiweek_end_date, 
                location = state_code, type, quantile, value = deaths)

# Add point forecast
cumulative_forecast <- cumulative_forecast %>%
  dplyr::bind_rows(cumulative_forecast %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point") %>%
                     dplyr::select(-quantile)) %>%
  dplyr::mutate(value = floor(value))


# Incident forecast -------------------------------------------

incident_forecast <- raw_weekly_forecast %>%
  # Filter to current and future epiweeks
  dplyr::filter(epiweek >= forecast_date_epiweek) %>%
  # Format for submission
  dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
  epiweek_to_date() %>%
  dplyr::mutate(type = "quantile",
                target = paste(epiweek - forecast_date_epiweek + 1,
                               "wk ahead inc death",
                               sep = " ")) %>%
  dplyr::select(forecast_date = date_created, target, target_end_date = epiweek_end_date, 
                location = state_code, type, quantile, value = deaths)

# Add point forecast
incident_forecast <- incident_forecast %>%
  dplyr::bind_rows(incident_forecast %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point") %>%
                     dplyr::select(-quantile)) %>%
  dplyr::mutate(value = floor(value))


# Bind cumulative and incident --------------------------------------------

out <- dplyr::bind_rows(incident_forecast, cumulative_forecast)

return(out)
}



