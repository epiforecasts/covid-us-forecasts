# Format all Rt regional estimates ----------------------------------------


# Packages ----------------------------------------------------------------

require(here)
require(stringr)
require(dplyr)
require(purrr)
require(tigris)

# Control parameters ------------------------------------------------------

# Updating latest forecast
forecast_date <- Sys.Date()
forecast_dir <- here::here("rt-forecast")  # Assumes forecasts are in national and regional subfolders

# Updating old forecasts
# forecast_date <- "2020-06-22"
# forecast_dir <- here::here("rt-forecast", "out-of-date") 

# Update US data saved in /data ----------------------------------------------------------

source(here::here("utils", "get-us-data.R"))

state_data_cumulative <- get_us_deaths(data = "cumulative")

state_data_daily <- get_us_deaths(data = "daily")


# Find forecasts ----------------------------------------------------------

forecasts <- c(list.dirs(file.path(forecast_dir, "state"), recursive = FALSE),
               list.dirs(file.path(forecast_dir, "national"), recursive = FALSE))

names(forecasts) <- forecasts %>%
  stringr::str_remove(file.path(forecast_dir, "state/")) %>%
  stringr::str_remove(file.path(forecast_dir, "national/"))


# Load formatting function ------------------------------------------------

source(here::here("rt-forecast", "format-rt-fn.R"))

# Extract forecasts -------------------------------------------------------

region_forecasts <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_rt_forecast(loc = .x, loc_name = .y,
                                                         forecast_date = forecast_date,
                                                         forecast_adjustment = 11 + 5,
                                                         horizon_weeks = 5,
                                                         state_data_cumulative = state_data_cumulative,
                                                         state_data_daily = state_data_daily
                                                         ))

region_forecasts_samples <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_rt_forecast(loc = .x, loc_name = .y,
                                                         forecast_date = forecast_date,
                                                         forecast_adjustment = 11 + 5,
                                                         horizon_weeks = 5,
                                                         state_data_cumulative = state_data_cumulative,
                                                         state_data_daily = state_data_daily, 
                                                         samples = TRUE
                                    ))



# Add state codes to csv submission
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

# Save forecast -----------------------------------------------------------
# Dated
readr::write_csv(region_forecasts,
                 paste0("rt-forecast/submission-files/dated/", forecast_date, "-rt-forecast-submission.csv"))
#Latest
readr::write_csv(region_forecasts,
                 paste0("rt-forecast/submission-files/latest-rt-forecast-submission.csv"))

saveRDS(region_forecasts_samples, 
        paste0("rt-forecast/submission-samples/", forecast_date, "-rt-forecast-samples.rds"))
