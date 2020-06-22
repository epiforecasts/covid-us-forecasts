# Forecasting using timeseries models from daily US deaths data
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

# Function to return an ensemble of time-series forecasts of deaths 
# Ensembles are averaged by equal weights
# This function only includes autoregressive methods (no external predictors)
# 
# Arguments
# data : dataframe - daily data including date, state, and death count
# horizon_weeks : numeric - number of days to forecast over
# format : logical - to return forecast formatted in quantiles appropriate for US forecast submission
# quantiles_out : numeric - if format == TRUE, vector of quantile probabilities to return

# Function to forecast ----------------------------------------------------

ts_deaths_only_forecast <- function(data, 
                                      sample_count, horizon_weeks, 
                                      format = FALSE, quantiles_out = NULL){
  
  data_weekly <- data %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6))) %>%
    group_by(state, week) %>%
    summarise(deaths = sum(deaths)) %>%
    filter(week < max(week))
    
  
  y = data_weekly$deaths
  
  # Forecast
  death_forecast <- data_weekly %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = y[max(1, length(y) - 6):length(y)],
                                                 samples = sample_count, 
                                                 horizon = horizon_weeks,
                                                 model_params = list(models = "aefz", weights = "equal"),
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
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$date)
  forecast_weeks <- seq.Date(from = as.Date(max(data_weekly$week)+7), by = 7, length.out = horizon_weeks)
  
  
  out <- quantile %>%
    select(state, date, quantile, "deaths" = 3) %>%
    mutate(date = recode(date, !!! setNames(forecast_weeks, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())
  
  return(out)
}
}




