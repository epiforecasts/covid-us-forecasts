# Forecasting using timeseries models from daily US deaths data
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

# Function to return an ensemble of time-series forecasts of deaths 
# Ensembles are averaged by equal weights
# This function only includes autoregressive methods (no external predictors)
# 
# Arguments
# data : dataframe - columns should be date, state, and death count
# horizon_days : numeric - number of days to forecast over
# models : character - string of model names as in forecastHybrid:  a, e, n, s, t = auto.arima, ets, nnetar, stlm and tbats
# format : logical - to return forecast formatted in quantiles appropriate for US forecast submission
# quantiles_out : numeric - if format == TRUE, vector of quantile probabilities to return

# Function to forecast ----------------------------------------------------

ts_deaths_only_forecast <- function(data, 
                                      sample_count, horizon_days, models, 
                                      format = FALSE, quantiles_out = NULL){
  
  # Forecast
  death_forecast <- data %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = .x$deaths,
                                                 samples = sample_count, 
                                                 horizon = horizon_days,
                                                 model_params = list(models = models))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "date")
 
  if(format == FALSE){
  return(death_forecast)
  }
  
  if(format == TRUE){
  # Get quantiles
  quantile <- death_forecast %>%
    group_by(state, date) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$date)
  forecast_dates <- seq(max(data$date)+1, by = 1, to = as.Date(max(data$date)+horizon_days))
  
  out <- quantile %>%
    select(state, date, quantile, "deaths" = 3) %>%
    mutate(date = recode(date, !!! setNames(forecast_dates, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())
  
  return(out)
}
}




