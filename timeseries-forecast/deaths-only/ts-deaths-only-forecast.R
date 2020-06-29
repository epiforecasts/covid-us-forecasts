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
    mutate(epiweek = lubridate::epiweek(date),
           date = NULL) %>%
    group_by(state, epiweek) %>%
    summarise(deaths = sum(deaths))
  
  right_truncate_date <- max(data_weekly_full$epiweek) - right_truncate_weeks
  
  data_weekly <- data_weekly_full %>%
    filter(epiweek <= right_truncate_date)
  
  # Set up forecasting vs historical dates
  # Forecasting = next week + horizon_weeks
  forecast_weeks <- seq(from = max(data_weekly$epiweek) + 1, by = 1, length.out = horizon_weeks)
  # Historical = last 6 weeks of data
  historical_weeks <- data_weekly %>%
    ungroup() %>%
    filter(!epiweek %in% forecast_weeks) %>%
    select(epiweek) %>%
    filter(epiweek > max(epiweek) - 6) %>%
    unique() %>%
    pull(epiweek)
  
  
  # Forecast 
  death_forecast <- data_weekly %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, epiweek %in% historical_weeks) %>%
                                                   pull("deaths"),
                                                 samples = sample_count, 
                                                 horizon = horizon_weeks,
                                                 model_params = list(models = "aez", weights = "equal"),
                                                 forecast_params = list(PI.combination = "mean"))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "epiweek")
 
  if(format == FALSE){
  return(death_forecast)
  }
  
  if(format == TRUE){
  # Get quantiles
  quantile <- death_forecast %>%
    group_by(state, epiweek) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$epiweek)
  
  out <- quantile %>%
    select(state, epiweek_target = epiweek, quantile, "deaths" = 3) %>%
    mutate(epiweek_target = recode(epiweek_target, !!! setNames(forecast_weeks, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "deaths_only",
           date_created = Sys.Date())
  
  return(out)
}
}




