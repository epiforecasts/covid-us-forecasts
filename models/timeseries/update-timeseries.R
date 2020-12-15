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
deaths_state <- get_us_deaths(data = "daily") %>% 
  filter(date <= forecast_date)

deaths_national <- deaths_state %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  mutate(state = "US")

cases_state <- get_us_cases(data = "daily") %>% 
  filter(date <= forecast_date)

cases_national <- cases_state %>%
  group_by(date) %>%
  summarise(cases = sum(cases), .groups = "drop_last") %>%
  mutate(state = "US")

# Forecast with case regressor --------------------------------------------
# State forecast
state_deaths_on_cases_forecast <- deaths_on_cases_forecast(case_data = cases_state,
                                                           deaths_data = deaths_state)

# National forecast
national_deaths_on_cases_forecast <- deaths_on_cases_forecast(case_data = cases_national,
                                                              deaths_data = deaths_national)
# Bind
deaths_on_cases_forecast <- rbindlist(list(national_deaths_on_cases_forecast, 
                                           state_deaths_on_cases_forecast))

readr::write_csv(deaths_on_cases_forecast, here("models", "timeseries", "data", "samples",
                                                paste0(forecast_date, ".csv")))

# Save formatted timeseries -----------------------------------------------
# save formatted forecasts 
source(here::here("utils", "format-forecast-us.R"))
formatted_forecasts <- format_forecast_us(forecasts = deaths_on_cases_forecast,
                                          forecast_date = forecast_date, 
                                          submission_date = forecast_date,
                                          shrink_per = 0)

dated_submission <- here("models", "timeseries", "data", "submission", "dated")
fwrite(formatted_forecasts, here("models", "timeseries", "data", "submission", "latest.csv"))
fwrite(formatted_forecasts, paste0(dated_submission, "/", forecast_date, ".csv"))
