# Packages ----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(readr)
library(here)
library(data.table)

# forecast date -----------------------------------------------------------
forecast_date <- readRDS(here("data", "target_date.rds"))

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("models", "timeseries", "utils", "deaths-on-cases-forecast.R"))

# Get data
deaths_state <- get_us_data(data = "deaths", 
                            include_national = TRUE,
                            incident = TRUE) %>% 
  filter(date <= forecast_date) %>%
  rename(deaths = value)

cases_state <- get_us_data(data = "cases", 
                           include_national = TRUE,
                           incident = TRUE) %>% 
  filter(date <= forecast_date) %>%
  rename(cases = value)

# Forecast with case regressor --------------------------------------------
deaths_on_cases_forecast <- deaths_on_cases_forecast(case_data = cases_state,
                                                           deaths_data = deaths_state)
samples_dir <- here("models", "timeseries", "data", "samples")
if (!dir.exists(samples_dir)) {
  dir.create(samples_dir, recursive = TRUE)
}
readr::write_csv(deaths_on_cases_forecast, file.path(samples_dir, paste0(forecast_date, ".csv")))

# Save formatted timeseries -----------------------------------------------
source(here::here("utils", "format-forecast-us.R"))
formatted_forecasts <- format_forecast_us(forecasts = deaths_on_cases_forecast,
                                          forecast_date = forecast_date, 
                                          submission_date = forecast_date,
                                          shrink_per = 0)

submission_dir <- here("models", "timeseries", "data", "submission")
if (!dir.exists(submission_dir)) {
  dir.create(submission_dir, recursive = TRUE)
}
fwrite(formatted_forecasts, paste0(submission_dir, "/", forecast_date, ".csv"))
