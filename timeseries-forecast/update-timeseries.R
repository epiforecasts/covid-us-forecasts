# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get_us_data.R"))
source(here::here("timeseries-forecast", "deaths-only", "ts-deaths-only-forecast.R"))
source(here::here("timeseries-forecast", "deaths-on-cases", "ts-deaths-on-cases-forecast.R"))
source(here::here("timeseries-forecast", "format-timeseries-forecast.R"))

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

sample_count = 1000
horizon_days = 40
models = "aez"
quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

# Forecast with deaths only -----------------------------------------------

# State forecast
state_forecast <- ts_deaths_only_forecast(data = deaths_state, 
                                            sample_count = sample_count, 
                                            horizon_days = horizon_days,
                                            models = models, 
                                            format = TRUE,
                                            quantiles_out = quantiles_out)

# National forecast
national_forecast <- ts_deaths_only_forecast(data = deaths_national, 
                                               sample_count = sample_count, 
                                               horizon_days = horizon_days,
                                               models = models, 
                                               format = TRUE,
                                               quantiles_out = quantiles_out)

# Bind and save daily forecast
daily_forecast_deaths_only <- bind_rows(national_forecast, state_forecast)
saveRDS(daily_forecast_deaths_only, here::here("timeseries-forecast", "deaths-only", paste0(Sys.Date(), "-daily-deaths-only.rds")))
saveRDS(daily_forecast_deaths_only, here::here("timeseries-forecast", "deaths-only", "latest-daily-deaths-only.rds"))

# Format to weekly and save for ensemble
incident_deaths_only <- format_timeseries_forecast(model_type = "deaths-only", weekly_count = "incident")
readr::write_csv(incident_deaths_only, here::here("timeseries-forecast", "deaths-only", "latest-weekly-inc-deaths-only.csv"))

cumulative_deaths_only <- format_timeseries_forecast(model_type = "deaths-only", weekly_count = "cumulative")
readr::write_csv(cumulative_deaths_only, here::here("timeseries-forecast", "deaths-only", "latest-weekly-cum-deaths-only.csv"))


# Forecast with case regressor --------------------------------------------

# State forecast
state_forecast_xreg <- ts_deaths_on_cases_forecast(case_data = cases_state,
                                                    deaths_data = deaths_state,
                                                    case_quantile = 0.5,
                                                    sample_count = sample_count, 
                                                    horizon_days = horizon_days,
                                                    models = models, 
                                                    format = TRUE, 
                                                    quantiles_out = quantiles_out)

# National forecast
national_forecast_xreg <- ts_deaths_on_cases_forecast(case_data = cases_national,
                                                      deaths_data = deaths_national,
                                                      case_quantile = 0.5,
                                                      sample_count = sample_count, 
                                                      horizon_days = horizon_days,
                                                      models = models, 
                                                      format = TRUE, 
                                                      quantiles_out = quantiles_out)

# Bind and save
daily_forecast_deaths_on_cases <- bind_rows(national_forecast_xreg, state_forecast_xreg)
saveRDS(daily_forecast_deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases", paste0(Sys.Date(), "-daily-deaths-on-cases.rds")))
saveRDS(daily_forecast_deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases", "latest-daily-deaths-on-cases.rds"))

# Format to weekly and save for ensemble

incident_deaths_on_cases <- format_timeseries_forecast(model_type = "deaths-on-cases", weekly_count = "incident")
readr::write_csv(incident_deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases", "latest-weekly-inc-deaths-on-cases.csv"))

cumulative_deaths_on_cases <- format_timeseries_forecast(model_type = "deaths-on-cases", weekly_count = "cumulative")
readr::write_csv(cumulative_deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases", "latest-weekly-cum-deaths-on-cases.csv"))


