# Update log transformed timeseries
source(here::here("utils", "get-us-data.R"))
source(here::here("timeseries-forecast", "deaths-on-cases", "ts-deaths-on-cases-forecast.R"))
source(here::here("timeseries-forecast", "deaths-log-cases", "log-log.R"))
source(here::here("timeseries-forecast", "deaths-log-cases", "log-lin.R"))

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

# Forecast ----------------------------------------------------------------
# Raw untransformed data
## Forecast
national_deaths_on_cases_forecast <- ts_deaths_on_cases_forecast(case_data = cases_national,
                                                                 deaths_data = deaths_national,
                                                                 case_quantile = case_quantile,
                                                                 sample_count = sample_count, 
                                                                 horizon_weeks = horizon_weeks,
                                                                 right_truncate_weeks = right_truncate_weeks,
                                                                 format = TRUE, 
                                                                 quantiles_out = quantiles_out)

## Save
saveRDS(national_deaths_on_cases_forecast, here::here("timeseries-forecast", "deaths-log-cases", "latest-weekly-raw.rds"))


# Log cases / raw deaths
## Forecast
national_loglin_forecast <- ts_loglin_forecast(case_data = cases_national,
                                                        deaths_data = deaths_national,
                                                        case_quantile = case_quantile,
                                                        sample_count = sample_count, 
                                                        horizon_weeks = horizon_weeks,
                                                        right_truncate_weeks = right_truncate_weeks,
                                                        format = TRUE, 
                                                        quantiles_out = quantiles_out)

## Save
saveRDS(national_loglin_forecast, here::here("timeseries-forecast", "deaths-log-cases", "latest-weekly-log-lin.rds"))



# Log cases / log deaths
# Forecast
national_loglog_forecast <- ts_loglog_forecast(case_data = cases_national,
                                                                 deaths_data = deaths_national,
                                                                 case_quantile = case_quantile,
                                                                 sample_count = sample_count, 
                                                                 horizon_weeks = horizon_weeks,
                                                                 right_truncate_weeks = right_truncate_weeks,
                                                                 format = TRUE, 
                                                                 quantiles_out = quantiles_out)

## Save
saveRDS(national_loglog_forecast, here::here("timeseries-forecast", "deaths-log-cases", "latest-weekly-log-log.rds"))


