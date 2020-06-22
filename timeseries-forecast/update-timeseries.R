# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get_us_data.R"))
source(here::here("timeseries-forecast", "deaths-only", "ts_deaths_only_forecast.R"))
source(here::here("timeseries-forecast", "deaths-on-cases", "ts_deaths_on_cases_forecast.R"))

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

sample_count = 10
horizon_days = 7
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

# Bind and save
latest_deaths_only <- bind_rows(national_forecast, state_forecast)
saveRDS(latest_deaths_only, paste0("timeseries-forecast/deaths-only/", Sys.Date(), ".rds"))
saveRDS(latest_deaths_only, "timeseries-forecast/deaths-only/latest.rds")

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
latest_deaths_on_cases <- bind_rows(national_forecast_xreg, state_forecast_xreg)
saveRDS(latest_deaths_on_cases, paste0("timeseries-forecast/deaths-on-cases/", Sys.Date(), ".rds"))
saveRDS(latest_deaths_on_cases, "timeseries-forecast/deaths-on-cases/latest.rds")
