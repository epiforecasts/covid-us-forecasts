# Forecasting using timeseries models from daily US cases or deaths data
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

# Function to return an ensemble of time-series forecasts
# Ensembles are averaged by equal weights
# This function only includes autoregressive methods (no external predictors)
# 
# Arguments
# data : dataframe - columns should be date, state, and case/death count
# target_variable : character string - "cases" or "deaths"
# horizon_days : numeric - number of days to forecast over
# models : character string - model names as in forecastHybrid:  a, e, n, s, t = auto.arima, ets, nnetar, stlm and tbats
# format : logical - to return forecast formatted in quantiles appropriate for US forecast submission
# quantiles_out : numeric - if format == TRUE, vector of quantile probabilities to return

# Function to forecast ----------------------------------------------------

fit_timeseries_forecast <- function(data, target_variable, sample_count, horizon_days, models, format, quantiles_out){
  
  states = unique(data$state)
  forecast_dates <- seq(max(data$date)+1, by = 1, to = as.Date(max(data$date)+horizon_days))
  
  # Forecast
  target_forecast <- data %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = .x$target_variable,
                                                 samples = sample_count, 
                                                 horizon = horizon_days,
                                                 model_params = list(models = models))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "date")
 
  if(format == FALSE){
  return(target_forecast)
  }
  
  if(format == TRUE){
  # Get quantiles
  quantile <- target_forecast %>%
    group_by(state, date) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$date)
  
  out <- quantile %>%
    select(state, date, quantile, target_variable = 3) %>%
    mutate(date = recode(date, !!! setNames(forecast_dates, dates_from)),
           target_variable = ifelse(target_variable < 0, 0, target_variable),
           target_variable = round(target_variable))
  
  return(out)
}
}




