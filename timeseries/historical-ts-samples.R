# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("timeseries-forecast", "deaths-only", "ts-deaths-only-forecast.R"))
source(here::here("timeseries-forecast", "deaths-on-cases", "ts-deaths-on-cases-forecast.R"))

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



# Historical submissions --------------------------------------------------

# Set historical timepoint for which to get samples as if forecast on that date
submission_dates <- c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20")

for(i in c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20")){
  submission_date <- i
  
# Set forecast parameters -------------------------------------------------
weeks_into_past <- lubridate::epiweek(Sys.Date()) - lubridate::epiweek(i)
right_truncation <- 1 # weeks
right_truncate_weeks <- weeks_into_past + right_truncation

sample_count <- 1000
horizon_weeks <- 4
case_quantile <- 0.5


# Forecast with deaths only -----------------------------------------------

# State forecast
state_deaths_only_forecast <- ts_deaths_only_forecast(data = deaths_state, 
                                                      sample_count = sample_count, 
                                                      horizon_weeks = horizon_weeks,
                                                      right_truncate_weeks = right_truncate_weeks)


# National forecast
national_deaths_only_forecast <- ts_deaths_only_forecast(data = deaths_national, 
                                                         sample_count = sample_count, 
                                                         horizon_weeks = horizon_weeks,
                                                         right_truncate_weeks = right_truncate_weeks)

# Bind and save daily forecast
deaths_only_forecast <- bind_rows(national_deaths_only_forecast, state_deaths_only_forecast) %>%
  mutate(submission_date = i)

saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "deaths-only", "raw-samples", "dated",
                                         paste0(i, "-samples-weekly-deaths-only.rds")))



# Forecast with case regressor --------------------------------------------

# State forecast
state_deaths_on_cases_forecast <- ts_deaths_on_cases_forecast(case_data = cases_state,
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
deaths_on_cases_forecast <- bind_rows(national_deaths_on_cases_forecast, state_deaths_on_cases_forecast) %>%
  mutate(submission_date = i)

saveRDS(deaths_on_cases_forecast, here::here("timeseries-forecast", "deaths-on-cases", "raw-samples", "dated",
                                         paste0(i, "-samples-weekly-deaths-on-cases.rds")))

}
