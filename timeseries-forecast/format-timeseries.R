# Format timeseries in output ready for submission (or ensembling)

# Arguments with examples
# model_type = c("deaths-only", "deaths-on-cases")
# right_truncate_weeks <- 1 [as used in forecasting: to prevent over-adding cumulative data]

format_timeseries <- function(model_type, right_truncate_weeks){
  
# Raw data set up -------------------------------------------------------------

# Get state codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US"))

# Read in forecast
raw_weekly_forecast <- readRDS(here::here("timeseries-forecast", model_type,
                                     paste0("latest-weekly-", model_type, ".rds")))

forecast_date <- unique(raw_weekly_forecast$date_created)

# Set epiweek to target date conversion
forecast_date_epiweek <- lubridate::epiweek(forecast_date)

epiweek_to_date <- tibble::tibble(date = seq.Date(from = (forecast_date-1), by = 1, length.out = 42)) %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date),
                day = weekdays(date)) %>%
  dplyr::filter(day == "Saturday") %>%
  dplyr::select(target_end_date = date, epiweek_target = epiweek)



# Cumulative formatting ----------------------------------------------------
  
# Get cumulative weekly data
cumulative_data <- readRDS(here::here("data", "deaths-data-cumulative.rds")) %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date))

# State cumulative data
cumulative_deaths_state <- cumulative_data %>%
  dplyr::group_by(epiweek) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup()

# National cumulative data
cumulative_deaths_national <- cumulative_data %>%
  dplyr::group_by(epiweek) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::summarise(deaths = sum(deaths),
                   state = "US") %>%
  dplyr::ungroup()

# Bind
cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)

# Filter to last week of data shown to model
last_week_cumulative_deaths <- cumulative_deaths %>%
  dplyr::filter(epiweek == max(epiweek)-right_truncate_weeks) %>%
  dplyr::select(-epiweek, -date)

# Create cumulative forecast
cumulative_forecast <- raw_weekly_forecast %>%
  dplyr::group_by(quantile, state) %>%
  dplyr::mutate(deaths = cumsum(deaths)) %>%
  ungroup()
  
# Join historical cumulative deaths to forecasts  
cumulative_forecast <- cumulative_forecast %>%  
  dplyr::left_join(last_week_cumulative_deaths, by = "state") %>%
  dplyr::mutate(deaths = deaths.x + deaths.y) %>%
  # Filter to current and future epiweeks
  dplyr::filter(epiweek_target >= forecast_date_epiweek) %>%
  # Format for submission
  dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
  dplyr::left_join(epiweek_to_date, by = "epiweek_target") %>%
  dplyr::mutate(forecast_date = date_created,
                value = deaths,
                type = "quantile",
                target = paste(epiweek_target - forecast_date_epiweek + 1,
                               "wk ahead cum death",
                               sep = " "),
                location = state_code) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)

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
  dplyr::filter(epiweek_target >= forecast_date_epiweek) %>%
  # Format for submission
  dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
  dplyr::left_join(epiweek_to_date, by = "epiweek_target") %>%
  dplyr::mutate(forecast_date = date_created,
                value = deaths,
                type = "quantile",
                target = paste(epiweek_target - forecast_date_epiweek + 1,
                               "wk ahead inc death",
                               sep = " "),
                location = state_code) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)

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



# Run function for each model type ----------------------------------------

# Deaths only

model_type <- "deaths-only"
deaths_only <- format_timeseries(right_truncate_weeks = 1, model_type = model_type)
forecast_date <- unique(deaths_only$forecast_date)
# Save under "latest"
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, 
                                         "submission-files",
                                         paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, "submission-files", 
                                         paste0(forecast_date, "-", model_type, ".csv")))
                

# Deaths on cases

model_type <- "deaths-on-cases"
deaths_on_cases <- format_timeseries(right_truncate_weeks = 1, model_type = model_type)
forecast_date <- unique(deaths_on_cases$forecast_date)

# Save under "latest"
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, 
                                             "submission-files",
                                             paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, "submission-files", 
                                             paste0(forecast_date, "-", model_type, ".csv")))





