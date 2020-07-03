# Forecasting using timeseries models from daily US deaths data
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

# Function to return an ensemble of time-series forecasts of deaths 
# Ensembles are averaged by equal weights
# This function only includes autoregressive methods (no external predictors)
# 
# Arguments
# data : dataframe - daily data including date, state, and death count
# horizon_days : numeric - number of weeks to forecast over
# right_truncate_days : numeric - number of weeks (dates) to remove from most recent date in data
# format : logical - to return forecast formatted in quantiles appropriate for US forecast submission
# quantiles_out : numeric - if format == TRUE, vector of quantile probabilities to return

# Function to forecast ----------------------------------------------------

ts_deaths_only_daily <- function(data, 
                                      sample_count, horizon_days, right_truncate_days,
                                      format = FALSE, quantiles_out = NULL){
  
  # Set up data and truncate
  data_weekly_full <- data
  
  right_truncate_date <- max(data_weekly_full$date) - right_truncate_days
  
  data_weekly <- data_weekly_full %>%
    filter(date <= right_truncate_date)
  
  # Set up forecasting vs historical dates
  # Forecasting dates
  forecast_dates <- seq.Date(from = max(data_weekly$date) + 1, by = 1, length.out = horizon_days)
  # Historical = last 3 weeks of data
  historical_dates <- data_weekly %>%
    ungroup() %>%
    filter(!date %in% forecast_dates) %>%
    select(date) %>%
    filter(date > max(date) - 21) %>%
    unique() %>%
    pull(date)
  
  
  # Forecast 
  death_forecast <- data_weekly %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_dates) %>%
                                                   pull("deaths"),
                                                 samples = sample_count, 
                                                 horizon = horizon_days,
                                                 model_params = list(models = "aez", weights = "equal"),
                                                 forecast_params = list(PI.combination = "mean"))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "date")
 
  if(format == FALSE){
  return(death_forecast)
  }
  
  if(format == TRUE){
  # Get quantiles
  quantile <- death_forecast %>%
    group_by(state, date) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$date)
  
  out <- quantile %>%
    select(state, date_target = date, quantile, "deaths" = 3) %>%
    mutate(date_target = recode(date_target, !!! setNames(forecast_dates, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())
  
  return(out)
}
}




