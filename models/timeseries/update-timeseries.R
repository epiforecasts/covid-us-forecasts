# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("models", "timeseries", "model.R"))

# Get data
deaths_state <- get_us_deaths(data = "daily")
deaths_national <- deaths_state %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  mutate(state = "US")

cases_state <- get_us_cases(data = "daily")
cases_national <- cases_state %>%
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
national_deaths_on_cases_forecast <- ts_deaths_on_cases_forecast(case_data = cases_national,
                                                                 deaths_data = deaths_national,
                                                                 case_quantile = case_quantile,
                                                                 sample_count = sample_count, 
                                                                 horizon_weeks = horizon_weeks,
                                                                 right_truncate_weeks = right_truncate_weeks)

# Bind and save
deaths_on_cases_forecast <- bind_rows(national_deaths_on_cases_forecast, state_deaths_on_cases_forecast)


# To format forecasts ready for ensembling and submission, go to "run-format-timeseries.R"