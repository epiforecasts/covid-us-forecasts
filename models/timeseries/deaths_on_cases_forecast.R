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
# horizon_weeks = 7
# right_truncate_weeks = 1
# quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

deaths_on_cases_forecast <- function(case_data, deaths_data, case_quantile,
                                     sample_count, horizon_weeks, right_truncate_weeks){
    

# Set up cases ----------------------------------------------------------

  # Set up case data and truncate
  case_data_weekly_full <- case_data %>%
    mutate(epiweek = lubridate::epiweek(date),
           date = NULL) %>%
    group_by(state, epiweek) %>%
    summarise(cases = sum(cases), .groups = "drop_last") 
  
  right_truncate_date <- max(case_data_weekly_full$epiweek) - right_truncate_weeks
  
  case_data_weekly <- case_data_weekly_full %>%
    filter(epiweek <= right_truncate_date) 
  

# Set up dates -----------------------------------------------------------

  # Forecasting = next week + horizon_weeks
  forecast_weeks <- seq(from = max(case_data_weekly$epiweek)+1, by = 1, length.out = horizon_weeks)
  case_forecast_weeks <- seq(from = max(case_data_weekly$epiweek)+1, by = 1, length.out = horizon_weeks+2)
  
  # Historical = last 6 weeks of data
  historical_weeks <- case_data_weekly %>%
    ungroup() %>%
    filter(!epiweek %in% forecast_weeks) %>%
    select(epiweek) %>%
    filter(epiweek > max(epiweek) - 6) %>%
    unique() %>%
    pull(epiweek)

  

# Forecast cases ---------------------------------------------------------
    
  case_forecast <- suppressMessages(case_data_weekly %>%
      group_by(state) %>%
      group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, epiweek %in% historical_weeks) %>%
                                                     pull("cases"),
                                                   samples = sample_count, 
                                                   horizon = horizon_weeks+2,
                                                   model_params = list(models = "aez", weights = "equal",
                                                                       a.args = list()),
                                                   forecast_params = list(PI.combination = "mean"))) %>%
      mutate(sample = rep(1:sample_count)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "epiweek"))
    
    # Get quantile
      quantile <- case_forecast %>%
        group_by(state, epiweek) %>%
        group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile, na.rm = T))) %>%
        mutate(quantile = case_quantile) %>%
        ungroup()
      
      # Format
      case_dates_from <- unique(quantile$epiweek)[1:length(case_forecast_weeks)]
       
      case_forecast <- quantile %>%
        select(state, epiweek, "cases" = 3) %>%
        mutate(cases = ifelse(cases < 0, 0, cases),
               cases = round(cases)) %>%
        mutate(epiweek = recode(epiweek, !!! setNames(case_forecast_weeks, case_dates_from)))

      

# Forecast deaths ---------------------------------------------------------
      
      # Set deaths data and truncate
      deaths_data_weekly_full <- deaths_data %>%
        mutate(epiweek = lubridate::epiweek(date),
               date = NULL) %>%
        group_by(state, epiweek) %>%
        summarise(deaths = sum(deaths), .groups = "drop_last")
      
      deaths_data_weekly <- deaths_data_weekly_full %>%
        filter(epiweek <= right_truncate_date)
      
      # Join deaths and cases
      cases_deaths <- dplyr::bind_rows(case_data_weekly, case_forecast) %>%
        group_by(state) %>%
        mutate(cases_lag1 = dplyr::lag(cases, 1),
               cases_lag2 = dplyr::lag(cases, 2)) %>%
        filter(epiweek <= max(epiweek)-2) %>%
        ungroup() %>%
        full_join(deaths_data_weekly, by = c("state", "epiweek"))
      
      # Forecast deaths (y) using cases
      death_forecast <- suppressMessages(cases_deaths %>%
        group_by(state) %>%
        group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, epiweek %in% historical_weeks) %>%
                                                       pull("deaths"),
                                                     samples = sample_count, 
                                                     horizon = horizon_weeks,
                                                     model_params = list(models = "aez", weights = "equal",
                                                                         a.args = list(
                                                                         xreg = 
                                                                         as.matrix(
                                                                           filter(.x, epiweek %in% historical_weeks) %>%
                                                                           pull("cases"),
                                                                         filter(.x, epiweek %in% historical_weeks) %>%
                                                                           pull("cases_lag1"),
                                                                         filter(.x, epiweek %in% historical_weeks) %>%
                                                                           pull("cases_lag2")))
                                                                         ),
                                                     forecast_params = list(xreg = 
                                                                            as.matrix(
                                                                            filter(.x, epiweek %in% forecast_weeks) %>%
                                                                              pull("cases"),
                                                                            filter(.x, epiweek %in% forecast_weeks) %>%
                                                                              pull("cases_lag1"),
                                                                            filter(.x, epiweek %in% forecast_weeks) %>%
                                                                              pull("cases_lag2")),
                                                                            PI.combination = "mean")
                                                     )) %>%
        mutate(sample = rep(1:sample_count)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "epiweek"))
      
      

# Return forecast ---------------------------------------------------------

      # Format
      deaths_dates_from <- unique(death_forecast$epiweek)[1:length(forecast_weeks)]
      
      samples <- death_forecast %>%
        select(state, sample, epiweek_target = epiweek, deaths = value) %>%
        mutate(epiweek_target = recode(epiweek_target, !!! setNames(forecast_weeks, deaths_dates_from)),
               deaths = ifelse(deaths < 0, 0, deaths),
               deaths = round(deaths),
               model_type = "deaths_on_cases",
               date_created = Sys.Date())
      
      return(samples)

}




      
      

      
      
  