# Forecasting using timeseries models from daily US deaths data
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

# Function to return an ensemble of time-series forecasts of deaths 
# Ensembles are averaged by equal weights
# This function only includes autoregressive methods (no external predictors)
# 
# Arguments
# data : dataframe - daily data including date, state, and death count
# horizon_weeks : numeric - number of weeks to forecast over
# right_truncate_weeks : numeric - number of weeks (epiweeks) to remove from most recent date in data
# format : logical - to return forecast formatted in quantiles appropriate for US forecast submission
# quantiles_out : numeric - if format == TRUE, vector of quantile probabilities to return

# Function to forecast ----------------------------------------------------

ts_deaths_only_daily_fc <- function(data, 
                                    sample_count, 
                                    horizon_days, 
                                    right_truncate_days){

  # Truncate data
  right_truncate_date <- max(data$date) - right_truncate_days
  data <- data %>%
    filter(date <= right_truncate_date)
  
  # Set up forecasting vs historical dates
  # Forecasting = next week + horizon_weeks
  forecast_days <- seq(from = max(data$date) + 1, by = 1, length.out = horizon_days + right_truncate_days)
  
  # Historical = using last 6 weeks of data
  historical_days <- data %>%
    ungroup() %>%
    filter(!date %in% forecast_days) %>%
    select(date) %>%
    filter(date > max(date) - (6*7)) %>%
    unique() %>%
    pull(date)
  
  
  # Forecast 
  death_forecast <- data %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_days) %>%
                                                   pull("deaths"),
                                                 samples = sample_count, 
                                                 horizon = horizon_days + right_truncate_days,
                                                 model_params = list(models = "aez", weights = "equal"),
                                                 forecast_params = list(PI.combination = "mean"))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    tidyr::pivot_longer(cols = starts_with("..."), names_to = "date")
  
  # Format
  dates_from <- unique(death_forecast$date)
  
  samples <- death_forecast %>%
    select(state, sample, date_target = date, deaths = value) %>%
    mutate(date_target = recode(date_target, !!! setNames(forecast_days, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())

  return(samples)
  
}


