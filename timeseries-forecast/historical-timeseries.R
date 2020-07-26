# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("utils", "dates-to-epiweeks.R"))
source(here::here("timeseries-forecast", "deaths-only", "ts-deaths-only-forecast.R"))
source(here::here("timeseries-forecast", "deaths-on-cases", "ts-deaths-on-cases-forecast.R"))

# Get data
deaths_state <- get_us_deaths(data = "daily")

deaths_national <- deaths_state %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(state = "US")


cases_state <- get_us_cases(data = "daily")

cases_national <- cases_state %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(state = "US")


weeks_into_past <- 1
right_truncation <- 1

# Set forecast parameters -------------------------------------------------
right_truncate_weeks <- weeks_into_past + right_truncation

sample_count <- 1000
horizon_weeks <- 4

format <- TRUE
quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
case_quantile <- 0.5


# Forecast with deaths only -----------------------------------------------

# State forecast
state_deaths_only_forecast <- ts_deaths_only_forecast(data = deaths_state, 
                                                      sample_count = sample_count, 
                                                      horizon_weeks = horizon_weeks,
                                                      right_truncate_weeks = right_truncate_weeks,
                                                      format = TRUE,
                                                      quantiles_out = quantiles_out)

# National forecast
national_deaths_only_forecast <- ts_deaths_only_forecast(data = deaths_national, 
                                                         sample_count = sample_count, 
                                                         horizon_weeks = horizon_weeks,
                                                         right_truncate_weeks = right_truncate_weeks,
                                                         format = TRUE,
                                                         quantiles_out = quantiles_out)

# Bind and save daily forecast
deaths_only_forecast <- bind_rows(national_deaths_only_forecast, state_deaths_only_forecast) %>%
  mutate(date_created = date_created - (7 * weeks_into_past))

date_created <- pull(deaths_only_forecast, date_created)[1]

saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "deaths-only", "raw-rds",
                                         paste0(date_created, "-deaths-only.rds")))



# Forecast with case regressor --------------------------------------------

# State forecast
state_deaths_on_cases_forecast <- ts_deaths_on_cases_forecast(case_data = cases_state,
                                                              deaths_data = deaths_state,
                                                              case_quantile = case_quantile,
                                                              sample_count = sample_count, 
                                                              horizon_weeks = horizon_weeks,
                                                              right_truncate_weeks = right_truncate_weeks,
                                                              format = TRUE, 
                                                              quantiles_out = quantiles_out)

# National forecast
national_deaths_on_cases_forecast <- ts_deaths_on_cases_forecast(case_data = cases_national,
                                                                 deaths_data = deaths_national,
                                                                 case_quantile = case_quantile,
                                                                 sample_count = sample_count, 
                                                                 horizon_weeks = horizon_weeks,
                                                                 right_truncate_weeks = right_truncate_weeks,
                                                                 format = TRUE, 
                                                                 quantiles_out = quantiles_out)

# Bind and save
deaths_on_cases_forecast <- bind_rows(national_deaths_on_cases_forecast, state_deaths_on_cases_forecast) %>%
  mutate(date_created = date_created - (7 * weeks_into_past))

date_created <- pull(deaths_on_cases_forecast, date_created)[1]

saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "deaths-on-cases", "raw-rds",
                                         paste0(date_created, "-deaths-on-cases.rds")))




# Format ------------------------------------------------------------------
# Use format-timeseries-fn
# Below code uncorrected with dates-to-epiweeks fn


# format_timeseries <- function(model_type, right_truncate_weeks, date_created){
#   
# # Raw data set up -------------------------------------------------------------
# 
# # Get state codes
# state_codes <- tigris::fips_codes %>%
#   dplyr::select(state_code, state_name) %>%
#   unique() %>%
#   rbind(c("US", "US"))
# 
# # Read in forecast
# raw_weekly_forecast <- readRDS(here::here("timeseries-forecast", model_type, "raw-rds",
#                                           paste0(date_created, "-", model_type, ".rds")))
# 
# forecast_date <- unique(raw_weekly_forecast$date_created)
# 
# # Set epiweek to target date conversion
# forecast_date_epiweek <- lubridate::epiweek(forecast_date)
# 
# epiweek_to_date <- tibble::tibble(date = seq.Date(from = (forecast_date-1), by = 1, length.out = 42)) %>%
#   dplyr::mutate(epiweek = lubridate::epiweek(date),
#                 day = weekdays(date)) %>%
#   dplyr::filter(day == "Saturday") %>%
#   dplyr::select(target_end_date = date, epiweek_target = epiweek)
# 
# 
# 
# # Cumulative formatting ----------------------------------------------------
# 
# # --- Get cumulative weekly data ---
# cumulative_data <- get_us_deaths(data = "cumulative")
# 
# # State cumulative data
# cumulative_deaths_state <- cumulative_data %>%
#   dates_to_epiweek() %>%
#   dplyr::filter(epiweek_end == TRUE)
# 
# # National cumulative data
# cumulative_deaths_national <- cumulative_deaths_state %>%
#   dplyr::group_by(epiweek) %>%
#   dplyr::summarise(deaths = sum(deaths),
#                    state = "US",
#                    .groups = "drop_last") %>%
#   dplyr::ungroup()
# 
# # Bind
# cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)
# 
# # Filter to last week of data shown to model
# last_week_cumulative_deaths <- cumulative_deaths %>%
#   dplyr::filter(epiweek == max(epiweek)-right_truncate_weeks) %>%
#   dplyr::select(-epiweek, -date)
# 
# 
# # Create cumulative forecast
# cumulative_forecast <- raw_weekly_forecast %>%
#   dplyr::group_by(quantile, state) %>%
#   dplyr::mutate(deaths = cumsum(deaths)) %>%
#   ungroup()
# 
# # Join historical cumulative deaths to forecasts  
# cumulative_forecast <- cumulative_forecast %>%  
#   dplyr::left_join(last_week_cumulative_deaths, by = "state") %>%
#   dplyr::mutate(deaths = deaths.x + deaths.y) %>%
#   # Filter to current and future epiweeks
#   dplyr::filter(epiweek_target >= forecast_date_epiweek) %>%
#   # Format for submission
#   dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
#   dplyr::left_join(epiweek_to_date, by = "epiweek_target") %>%
#   dplyr::mutate(forecast_date = date_created,
#                 value = deaths,
#                 type = "quantile",
#                 target = paste(epiweek_target - forecast_date_epiweek + 1,
#                                "wk ahead cum death",
#                                sep = " "),
#                 location = state_code) %>%
#   dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)
# 
# # Add point forecast
# cumulative_forecast <- cumulative_forecast %>%
#   dplyr::bind_rows(cumulative_forecast %>%
#                      dplyr::filter(quantile == 0.5) %>%
#                      dplyr::mutate(type = "point") %>%
#                      dplyr::select(-quantile)) %>%
#   dplyr::mutate(value = floor(value))
# 
# 
# # Incident forecast -------------------------------------------
# 
# incident_forecast <- raw_weekly_forecast %>%
#   # Filter to current and future epiweeks
#   dplyr::filter(epiweek_target >= forecast_date_epiweek) %>%
#   # Format for submission
#   dplyr::left_join(state_codes, by = c("state" = "state_name")) %>%
#   dplyr::left_join(epiweek_to_date, by = "epiweek_target") %>%
#   dplyr::mutate(forecast_date = date_created,
#                 value = deaths,
#                 type = "quantile",
#                 target = paste(epiweek_target - forecast_date_epiweek + 1,
#                                "wk ahead inc death",
#                                sep = " "),
#                 location = state_code) %>%
#   dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)
# 
# # Add point forecast
# incident_forecast <- incident_forecast %>%
#   dplyr::bind_rows(incident_forecast %>%
#                      dplyr::filter(quantile == 0.5) %>%
#                      dplyr::mutate(type = "point") %>%
#                      dplyr::select(-quantile)) %>%
#   dplyr::mutate(value = floor(value))
# 
# 
# # Bind cumulative and incident --------------------------------------------
# 
# out <- dplyr::bind_rows(incident_forecast, cumulative_forecast)
# 
# return(out)
# }



# Run function for each model type ----------------------------------------

# Deaths only

model_type <- "deaths-only"
deaths_only <- format_timeseries(right_truncate_weeks = right_truncate_weeks, 
                                 model_type = model_type,
                                 date_created = date_created)
forecast_date <- unique(deaths_only$forecast_date)

# Save under forecast date
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, 
                                         "submission-files", "dated",
                                         paste0(forecast_date, "-", model_type, ".csv")))


# Deaths on cases

model_type <- "deaths-on-cases"
deaths_on_cases <- format_timeseries(right_truncate_weeks = right_truncate_weeks, 
                                     model_type = model_type,
                                     date_created = date_created)
forecast_date <- unique(deaths_on_cases$forecast_date)

# Save under forecast date
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, 
                                             "submission-files", "dated",
                                             paste0(forecast_date, "-", model_type, ".csv")))
