# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-deaths-only-daily.R"))
source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-deaths-on-cases-daily.R"))
source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-log-lin-daily.R"))
source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-log-log-daily.R"))

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
horizon_days <- 6
right_truncate_days <- 7
format <- TRUE
quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
case_quantile <- 0.5
  

# Forecast with deaths only -----------------------------------------------

## State forecast
state_deaths_only_daily <- ts_deaths_only_daily(data = deaths_state, 
                                            sample_count = sample_count, 
                                            horizon_days = horizon_days,
                                            right_truncate_days = right_truncate_days,
                                            format = TRUE,
                                            quantiles_out = quantiles_out)

## National forecast
national_deaths_only_daily <- ts_deaths_only_daily(data = deaths_national, 
                                             sample_count = sample_count, 
                                             horizon_days = horizon_days,
                                             right_truncate_days = right_truncate_days,
                                             format = TRUE,
                                             quantiles_out = quantiles_out)

## Bind and save daily forecast
#deaths_only_daily <- bind_rows(national_deaths_only_daily, state_deaths_only_daily)
saveRDS(deaths_only_daily, here::here("timeseries-forecast", "daily", paste0(Sys.Date(), "-daily-deaths-only.rds"))) saveRDS(deaths_only_forecast, here::here("timeseries-forecast", "daily", "raw-rds", "latest-daily-deaths-only.rds"))


# Forecast with case regressor --------------------------------------------

## State forecast
state_deaths_on_cases_daily <- ts_deaths_on_cases_daily(case_data = cases_state,
                                                        deaths_data = deaths_state,
                                                        case_quantile = case_quantile,
                                                        sample_count = sample_count, 
                                                        horizon_days = horizon_days,
                                                        right_truncate_days = right_truncate_days,
                                                        format = TRUE, 
                                                        quantiles_out = quantiles_out)

## National forecast
national_deaths_on_cases_daily <- ts_deaths_on_cases_daily(case_data = cases_national,
                                                           deaths_data = deaths_national,
                                                           case_quantile = case_quantile,
                                                           sample_count = sample_count, 
                                                           horizon_days = horizon_days,
                                                           right_truncate_days = right_truncate_days,
                                                           format = TRUE, 
                                                           quantiles_out = quantiles_out)

## Bind and save
deaths_on_cases_daily <- bind_rows(national_deaths_on_cases_daily, state_deaths_on_cases_daily)
#saveRDS(deaths_on_cases_daily, here::here("timeseries-forecast", "daily", "raw-rds", paste0(Sys.Date(), "-daily-deaths-on-cases.rds")))
saveRDS(deaths_on_cases_daily, here::here("timeseries-forecast", "daily", "latest-daily-deaths-on-cases.rds"))

# Forecast with logged case regressor --------------------------------------------

## State forecast
state_log_lin_daily <- ts_loglin_daily(case_data = cases_state,
                                        deaths_data = deaths_state,
                                        case_quantile = case_quantile,
                                        sample_count = sample_count, 
                                        horizon_days = horizon_days,
                                        right_truncate_days = right_truncate_days,
                                        format = TRUE, 
                                        quantiles_out = quantiles_out)

## National forecast
national_log_lin_daily <- ts_loglin_daily(case_data = cases_national,
                                           deaths_data = deaths_national,
                                           case_quantile = case_quantile,
                                           sample_count = sample_count, 
                                           horizon_days = horizon_days,
                                           right_truncate_days = right_truncate_days,
                                           format = TRUE, 
                                           quantiles_out = quantiles_out)

## Bind and save
log_lin_daily <- bind_rows(national_log_lin_daily, state_log_lin_daily)
#saveRDS(log_lin_daily, here::here("timeseries-forecast", "daily", paste0(Sys.Date(), "-daily-log-lin.rds")))
saveRDS(log_lin_daily, here::here("timeseries-forecast", "daily", "raw-rds", "latest-daily-log-lin.rds"))


# Forecast with log deaths and log cases regressor --------------------------------------------

## State forecast
state_log_log_daily <- ts_loglog_daily(case_data = cases_state,
                                                        deaths_data = deaths_state,
                                                        case_quantile = case_quantile,
                                                        sample_count = sample_count, 
                                                        horizon_days = horizon_days,
                                                        right_truncate_days = right_truncate_days,
                                                        format = TRUE, 
                                                        quantiles_out = quantiles_out)

## National forecast
national_log_log_daily <- ts_loglog_daily(case_data = cases_national,
                                                           deaths_data = deaths_national,
                                                           case_quantile = case_quantile,
                                                           sample_count = sample_count, 
                                                           horizon_days = horizon_days,
                                                           right_truncate_days = right_truncate_days,
                                                           format = TRUE, 
                                                           quantiles_out = quantiles_out)

## Bind and save
log_log_daily <- bind_rows(national_log_log_daily, state_log_log_daily)
#saveRDS(log_log_daily, here::here("timeseries-forecast", "daily", paste0(Sys.Date(), "-daily-log-log.rds")))
saveRDS(log_log_daily, here::here("timeseries-forecast", "daily", "raw-rds", "latest-daily-log-log.rds"))



# Plot / format -----------------------------------------------------------

# For immediate plotting of rds files, go to "plot-timeseries.R"
# To format forecasts ready for ensembling and submission, go to "format-timeseries.R"