# Update deaths forecasts from timeseries
library(magrittr); library(dplyr); library(readr); library(here)

# forecast date -----------------------------------------------------------
forecast_date <- Sys.Date()

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("models", "timeseries", "utils", "deaths-on-cases-forecast.R"))
source(here::here("models", "timeseries", "utils", "format-timeseries.R"))

# Get data
deaths_state <- get_us_deaths(data = "daily")
deaths_national <- deaths_state %>%
  filter(date <= forecast_date) %>% 
  group_by(date) %>%
  summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  mutate(state = "US")

cases_state <- get_us_cases(data = "daily")
cases_national <- cases_state %>%
  filter(date <= forecast_date) %>% 
  group_by(date) %>%
  summarise(cases = sum(cases), .groups = "drop_last") %>%
  mutate(state = "US")

# Set forecast parameters -------------------------------------------------
sample_count <- 1000
case_quantile <- 0.5

# 4 wk ahead forecast:
horizon_weeks <- 4
right_truncate_weeks <- 1

# Forecast with case regressor --------------------------------------------
# State forecast
state_deaths_on_cases_forecast <- deaths_on_cases_forecast(case_data = cases_state,
                                                           deaths_data = deaths_state,
                                                           case_quantile = case_quantile,
                                                           sample_count = sample_count, 
                                                           horizon_weeks = horizon_weeks,
                                                           right_truncate_weeks = right_truncate_weeks)

# National forecast
national_deaths_on_cases_forecast <- deaths_on_cases_forecast(case_data = cases_national,
                                                              deaths_data = deaths_national,
                                                              case_quantile = case_quantile,
                                                              sample_count = sample_count, 
                                                              horizon_weeks = horizon_weeks,
                                                              right_truncate_weeks = right_truncate_weeks)

# Bind
deaths_on_cases_forecast <- bind_rows(national_deaths_on_cases_forecast, state_deaths_on_cases_forecast)

readr::write_csv(deaths_on_cases_forecast, here("models", "timeseries", "data", "samples",
                                                paste0(forecast_date, ".csv")))

# Save formatted timeseries -----------------------------------------------
deaths_on_cases <- format_timeseries(deaths_on_cases_forecast,
                                     right_truncate_weeks = 1, 
                                     forecast_date = forecast_date,
                                     submission_date = forecast_date,
                                     quantiles_out = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))

# Save under "latest"
readr::write_csv(deaths_on_cases, here::here("model", "timeseries", "data", "submission",
                                             paste0("latest.csv")))
# Save under forecast date
readr::write_csv(deaths_on_cases, here::here("model", "timeseries", "data", "submission", "dated",
                                             paste0(forecast_date, ".csv")))
