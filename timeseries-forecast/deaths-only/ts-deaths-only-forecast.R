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

ts_deaths_only_forecast <- function(data, 
                                      sample_count, horizon_weeks, right_truncate_weeks,
                                      format = FALSE, quantiles_out = NULL){
  
  # Set up data and truncate
  data_weekly_full <- data %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6)),
           date = NULL) %>%
    group_by(state, week) %>%
    summarise(deaths = sum(deaths))
    
  right_truncate_date <- max(data_weekly_full$week) - 7 * right_truncate_weeks
  
  data_weekly <- data_weekly_full %>%
    filter(week <= right_truncate_date)
  
  # Set up forecasting vs historical dates
  # Forecasting = next week + horizon_weeks
  forecast_weeks <- seq.Date(from = as.Date(max(data_weekly$week)+7), by = 7, length.out = horizon_weeks)
  # Historical = last 6 weeks of data
  historical_weeks <- data_weekly %>%
    ungroup() %>%
    filter(!week %in% forecast_weeks) %>%
    select(week) %>%
    filter(week > max(week) - 42) %>%
    unique() %>%
    pull(week)
  
  
  # Forecast 
  death_forecast <- data_weekly %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, week %in% historical_weeks) %>% pull("deaths"),
                                                 samples = sample_count, 
                                                 horizon = horizon_weeks,
                                                 model_params = list(models = "aefz", weights = "equal"),
                                                 forecast_params = list(PI.combination = "mean"))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "week")
 
  if(format == FALSE){
  return(death_forecast)
  }
  
  if(format == TRUE){
  # Get quantiles
  quantile <- death_forecast %>%
    group_by(state, week) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$week)
  
  out <- quantile %>%
    select(state, week, quantile, "deaths" = 3) %>%
    mutate(week = recode(week, !!! setNames(forecast_weeks, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())
  
  return(out)
}
}




