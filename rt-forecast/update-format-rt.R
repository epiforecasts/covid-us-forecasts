# Format all Rt regional estimates ----------------------------------------


# Packages ----------------------------------------------------------------

require(here)
require(stringr)
require(dplyr)
require(purrr)
require(tigris)

# Control parameters ------------------------------------------------------

# Updating latest forecast
forecast_dir <- here::here("rt-forecast")  # Assumes forecasts are in national and regional subfolders

source(here::here("utils", "current-forecast-submission-date.R"))

## IGNORE UNLESS NEEDED: Getting and formatting past forecasts
#
# The very first submission, 2020-06-15, was saved in a non-standard file structure. To get forecast, set: 
# forecast_date <- "2020-06-15"
# forecast_dir <- here::here("rt-forecast", "out-of-date") 
# 
# Forecasts later than this (2020-06-22 onwards) are saved in the standard file structure


# Update US data saved in /data ----------------------------------------------------------

source(here::here("utils", "get-us-data.R"))

state_data_cumulative <- get_us_deaths(data = "cumulative")

state_data_daily <- get_us_deaths(data = "daily")

## Formatting historical forecasts:
# forecast_dates <- c("2020-06-22", "2020-06-28", "2020-07-05", "2020-07-13", "2020-07-19")
# submission_dates <- c("2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20")
# names(forecast_dates) <- submission_dates
# forecast_dir <- here::here("rt-forecast")

# for(i in 1:length(forecast_dates)){
#   
#   forecast_date <- forecast_dates[i]
#   submission_date <- submission_dates[i]
  
  message(paste0("Forecast on ", forecast_date, "; Submission ", submission_date))
  
# Find forecasts ----------------------------------------------------------

forecasts <- c(list.dirs(file.path(forecast_dir, "state"), recursive = FALSE),
               list.dirs(file.path(forecast_dir, "national"), recursive = FALSE))

names(forecasts) <- forecasts %>%
  stringr::str_remove(file.path(forecast_dir, "state//")) %>%
  stringr::str_remove(file.path(forecast_dir, "national//"))


# Load formatting function ------------------------------------------------

source(here::here("rt-forecast", "format-rt-fn.R"))
source(here::here("utils", "dates-to-epiweek.R"))

# Get forecast and format for subsmission -------------------------------------------------------
message("Saving submission files")

region_forecasts <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_rt_forecast(loc = .x, loc_name = .y,
                                                         forecast_date = forecast_date,
                                                         submission_date = submission_date,
                                                         forecast_adjustment = 11 + 5,
                                                         horizon_weeks = 5,
                                                         state_data_cumulative = state_data_cumulative,
                                                         state_data_daily = state_data_daily
                                                         ))

# Add state codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US"))

region_forecasts <- region_forecasts %>%
  dplyr::mutate(state_name = gsub("/", "", x = location)) %>%
  dplyr::left_join(state_codes, by = "state_name") %>%
  dplyr::mutate(location = state_code) %>%
  dplyr::select(-state_name, -state_code)

region_forecasts$forecast_date <- forecast_date

# Save submission
# Dated
readr::write_csv(region_forecasts,
                 paste0("rt-forecast/submission-files/dated/", forecast_date, "-rt-forecast-submission.csv"))
# Latest
readr::write_csv(region_forecasts,
                 paste0("rt-forecast/submission-files/latest-rt-forecast-submission.csv"))


# Get and save samples ------------------------------------------------------------
message("Saving samples")
region_forecasts_samples <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_rt_forecast(loc = .x, loc_name = .y,
                                                         forecast_date = forecast_date,
                                                         submission_date = submission_date,
                                                         forecast_adjustment = 11 + 5,
                                                         horizon_weeks = 5,
                                                         state_data_cumulative = state_data_cumulative,
                                                         state_data_daily = state_data_daily, 
                                                         samples = TRUE,
                                                         replace_missing_with_latest = FALSE
                                    )) %>%
  dplyr::mutate(forecast_date = forecast_date)


# Save samples
saveRDS(region_forecasts_samples, 
        paste0("rt-forecast/submission-samples/", forecast_date, "-rt-forecast-samples.rds"))


## Formatting historical forecasts:
# }
