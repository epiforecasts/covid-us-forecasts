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

ts_deaths_on_cases_forecast <- function(case_data, deaths_data, case_quantile,
                                           sample_count, horizon_days, right_truncate_days){
    

# Set up cases ----------------------------------------------------------

  # Set up case data and truncate
  
  right_truncate_date <- max(case_data$date) - right_truncate_days
  
  case_data <- case_data %>%
    filter(epiweek <= right_truncate_date) 
  

# Set up dates -----------------------------------------------------------

  # Forecasting = next week + horizon_weeks
  forecast_days <- seq(from = max(case_data$date)+1, by = 1, length.out = horizon_days + right_truncate_days)
  case_forecast_days <- seq(from = max(case_data$date)+1, by = 1, length.out = horizon_days + right_truncate_days)
  
  # Historical = last 6 weeks of data
  historical_days <- case_data %>%
    ungroup() %>%
    filter(!date %in% forecast_days) %>%
    select(date) %>%
    filter(date > max(date) - (6*7) &
             date <= right_truncate_date) %>%
    unique() %>%
    pull(date)

  

# Forecast cases ---------------------------------------------------------
   
  case_forecast <- case_data %>%
      group_by(state) %>%
      group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_days) %>%
                                                     pull("cases"),
                                                   samples = sample_count, 
                                                   horizon = horizon_days + right_truncate_days,
                                                   model_params = list(models = "aez", weights = "equal",
                                                                       a.args = list()),
                                                   forecast_params = list(PI.combination = "mean"))) %>%
      mutate(sample = rep(1:sample_count)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "date")
    
    # Get quantile
      quantile <- case_forecast %>%
        group_by(state, date) %>%
        group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile, na.rm = T))) %>%
        mutate(quantile = case_quantile) %>%
        ungroup()
      
      # Format
      case_dates_from <- unique(quantile$date)[1:length(case_forecast_days)]
       
      case_forecast <- quantile %>%
        select(state, date, "cases" = 3) %>%
        mutate(cases = ifelse(cases < 0, 0, cases),
               cases = round(cases)) %>%
        mutate(date = recode(date, !!! setNames(case_forecast_days, case_dates_from)))

      

# Forecast deaths ---------------------------------------------------------
      
      # Truncate
      deaths_data <- deaths_data %>%
        filter(date <= right_truncate_date) 
      
      
      # Join deaths and cases
      cases_deaths <- dplyr::bind_rows(case_data, case_forecast) %>%
        group_by(state) %>%
        mutate(cases_lag7 = dplyr::lag(cases, 7),
               cases_lag9 = dplyr::lag(cases, 9),
               cases_lag11 = dplyr::lag(cases, 11)) %>%
        ungroup() %>%
        full_join(deaths_data, by = c("state", "date"))
      
      # Forecast deaths (y) using cases
      death_forecast <- cases_deaths %>%
        group_by(state) %>%
        group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, date %in% historical_days) %>%
                                                       pull("deaths"),
                                                     samples = sample_count, 
                                                     horizon = horizon_days + right_truncate_days,
                                                     model_params = list(models = "aez", 
                                                                         weights = "equal",
                                                                         a.args = list(
                                                                          xreg =
                                                                           as.matrix(
                                                                               filter(.x, date %in% historical_days) %>%
                                                                                 pull("cases"),
                                                                               filter(.x, date %in% historical_days) %>%
                                                                                 pull("cases_lag7"),
                                                                               filter(.x, date %in% historical_days) %>%
                                                                                 pull("cases_lag9"),
                                                                               filter(.x, date %in% historical_days) %>%
                                                                                 pull("cases_lag11")
                                                                               ))),
                                                     forecast_params = list(xreg = 
                                                                              as.matrix(
                                                                                filter(.x, date %in% forecast_days) %>%
                                                                                  pull("cases"),
                                                                                filter(.x, date %in% forecast_days) %>%
                                                                                  pull("cases_lag7"),
                                                                                filter(.x, date %in% forecast_days) %>%
                                                                                  pull("cases_lag9"),
                                                                                filter(.x, date %in% forecast_days) %>%
                                                                                  pull("cases_lag11")
                                                                                ),
                                                                            PI.combination = "mean"))) %>%
        mutate(sample = rep(1:sample_count)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "date")
      
      

# Return forecast ---------------------------------------------------------

      # Format
      deaths_dates_from <- unique(death_forecast$date)[1:length(forecast_days)]
      
      samples <- death_forecast %>%
        select(state, sample, date_target = date, deaths = value) %>%
        mutate(date_target = recode(date_target, !!! setNames(forecast_days, deaths_dates_from)),
               deaths = ifelse(deaths < 0, 0, deaths),
               deaths = round(deaths),
               model_type = "deaths_on_cases",
               date_created = Sys.Date())
      
      return(samples)

}




      
      

      
      
  