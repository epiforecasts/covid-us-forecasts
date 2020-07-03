# Forecast deaths from cases using timeseries
# See <timeseries_death_forecast> for arguments
# 
# Additional arguments:
# case_data : dataframe with case data
# deaths_data : dataframe with deaths data
# case_quantile : numeric, which quantile to return from case forecasting to be used in the death forecast
# 
# 
# Example parameters:
# deaths_data <- get_us_deaths(data = "daily")
# case_data <- get_us_cases(data = "daily")
# case_quantile = 0.5
# sample_count = 1000
# horizon_days = 7
# right_truncate_weeks = 1
# quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

ts_deaths_on_cases_daily <- function(case_data, deaths_data, case_quantile,
                                        sample_count, horizon_days, right_truncate_days,
                                        format = FALSE, quantiles_out = NULL){
  
  
  # Set up cases ----------------------------------------------------------
  
  # Set up case data and truncate
  case_data_full <- case_data
  
  right_truncate_date <- max(case_data_full$date) - right_truncate_days
  
  case_data <- case_data_full %>%
    filter(date <= right_truncate_date) 
  
  
  # Set up dates -----------------------------------------------------------
  
  # Forecasting dates
  forecast_dates <- seq.Date(from = max(case_data$date)+1, by = 1, length.out = horizon_days)
  
  # Historical = last 3 weeks of data
  historical_dates <- case_data %>%
    ungroup() %>%
    filter(!date %in% forecast_dates) %>%
    select(date) %>%
    filter(date > max(date) - 21) %>% # Take last 3 weeks
    unique() %>%
    pull(date)
  
  
  # Forecast cases ---------------------------------------------------------
  
  case_forecast <- case_data %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_dates) %>%
                                                   pull("cases"),
                                                 samples = sample_count, 
                                                 horizon = horizon_days,
                                                 model_params = list(models = "aez", weights = "equal",
                                                                     a.args = list()),
                                                 forecast_params = list(PI.combination = "mean"))) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "date")
  
  # Get quantile
  quantile <- case_forecast %>%
    group_by(state, date) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile, na.rm = T))) %>%
    mutate(quantile = case_quantile) %>%
    ungroup()
  
  # Format
  dates_from <- unique(quantile$date)
  
  case_forecast <- quantile %>%
    select(state, date, "cases" = 3) %>%
    mutate(date = recode(date, !!! setNames(forecast_dates, dates_from)),
           cases = ifelse(cases < 0, 0, cases),
           cases = round(cases))
  
  
  
  # Forecast deaths ---------------------------------------------------------
  
  # Set deaths data and truncate
  deaths_data_full <- deaths_data
  
  deaths_data <- deaths_data_full %>%
    filter(date <= right_truncate_date)
  
  # Join deaths and cases
  cases_deaths <- dplyr::bind_rows(case_data, case_forecast) %>%
    full_join(deaths_data, by = c("state", "date"))
  
  # Forecast deaths (y) using cases
  death_forecast <- cases_deaths %>%
    group_by(state) %>%
    group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_dates) %>%
                                                   pull("deaths"),
                                                 samples = sample_count, 
                                                 horizon = horizon_days,
                                                 model_params = list(models = "aez", weights = "equal",
                                                                     a.args = list(
                                                                       xreg = filter(.x, date %in% historical_dates) %>%
                                                                         pull("cases"))
                                                 ),
                                                 forecast_params = list(xreg = filter(.x, date %in% forecast_dates) %>%
                                                                          pull("cases"),
                                                                        PI.combination = "mean")
    )) %>%
    mutate(sample = rep(1:sample_count)) %>%
    pivot_longer(cols = starts_with("..."), names_to = "date")
  
  
  
  # Return forecast ---------------------------------------------------------
  
  if(format == FALSE){
    return(death_forecast)
  }
  
  if(format == TRUE){
    # Get quantiles
    quantile <- death_forecast %>%
      group_by(state, date) %>%
      group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
      mutate(quantile = quantiles_out) %>%
      ungroup() %>%
      select(state, date_target = date, quantile, "deaths" = 3) %>%
      mutate(date_target = recode(date_target, !!! setNames(forecast_dates, dates_from)),
             deaths = ifelse(deaths < 0, 0, deaths),
             deaths = round(deaths),
             model_type = "deaths_on_cases",
             date_created = Sys.Date())
    
    return(quantile)
  }
}









