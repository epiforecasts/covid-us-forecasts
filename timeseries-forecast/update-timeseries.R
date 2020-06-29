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
  summarise(deaths = sum(deaths)) %>%
  mutate(state = "US")


cases_state <- get_us_cases(data = "daily")

cases_national <- cases_state %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(state = "US")



# Set forecast parameters -------------------------------------------------

sample_count <- 1000
horizon_weeks <- 6
right_truncate_weeks <- 1
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
deaths_only_forecast <- bind_rows(national_deaths_only_forecast, state_deaths_only_forecast)
saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "deaths-only", paste0(Sys.Date(), "-weekly-deaths-only.rds")))
saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "deaths-only", "latest-weekly-deaths-only.rds"))


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
deaths_on_cases_forecast <- bind_rows(national_deaths_on_cases_forecast, state_deaths_on_cases_forecast)
saveRDS(deaths_on_cases_forecast, here::here("timeseries-forecast", "deaths-on-cases", paste0(Sys.Date(), "-weekly-deaths-on-cases.rds")))
saveRDS(deaths_on_cases_forecast, here::here("timeseries-forecast", "deaths-on-cases", "latest-weekly-deaths-on-cases.rds"))


# For immediate plotting of rds files, go to "plot-timeseries.R"
# To format forecasts ready for ensembling and submission, go to "format-timeseries.R"