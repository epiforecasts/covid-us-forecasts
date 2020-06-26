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
# sample_count = 10
# horizon_weeks = 7
# models = "az"
# case_quantile = 0.5
# quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

ts_deaths_on_cases_forecast <- function(case_data, deaths_data, case_quantile,
                                           sample_count, horizon_weeks, right_truncate_weeks,
                                           format = FALSE, quantiles_out = NULL){
    

# Forecast cases ----------------------------------------------------------

  # Set up case data and truncate
  case_data_weekly_full <- case_data %>%
    mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6)),
           date = NULL) %>%
    group_by(state, week) %>%
    summarise(cases = sum(cases)) 

  
  right_truncate_date <- max(case_data_weekly_full$week) - 7 * right_truncate_weeks
  
  case_data_weekly <- case_data_weekly_full %>%
    filter(week <= right_truncate_date)
  
  # Set up dates
  # Forecasting = next week + horizon_weeks
  forecast_weeks <- seq.Date(from = as.Date(max(case_data_weekly$week)+7), by = 7, length.out = horizon_weeks)
  
  # Historical = last 6 weeks of data
  historical_weeks <- case_data_weekly %>%
    ungroup() %>%
    filter(!week %in% forecast_weeks) %>%
    select(week) %>%
    filter(week > max(week) - 42) %>%
    unique() %>%
    pull(week)
  
  # Forecast y
    case_forecast <- case_data_weekly %>%
      group_by(state) %>%
      group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, week %in% historical_weeks) %>% pull("cases"),
                                                   samples = sample_count, 
                                                   horizon = horizon_weeks,
                                                   model_params = list(models = "aefz", weights = "equal"),
                                                   forecast_params = list(PI.combination = "mean"))) %>%
      mutate(sample = rep(1:sample_count)) %>%
      pivot_longer(cols = starts_with("..."), names_to = "week")
    
    # Get quantile
      quantile <- case_forecast %>%
        group_by(state, week) %>%
        group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile, na.rm = T))) %>%
        mutate(quantile = case_quantile) %>%
        ungroup()
      
      # Set up dates
      dates_from <- unique(quantile$week)
      forecast_weeks <- seq.Date(from = as.Date(max(case_data_weekly$week)+7), by = 7, length.out = horizon_weeks)
      historical_weeks <- filter(case_data_weekly, !week %in% forecast_weeks) %>% pull(week) %>% unique()
      
      # Format
      case_forecast <- quantile %>%
        select(state, week, "cases" = 3) %>%
        mutate(week = recode(week, !!! setNames(forecast_weeks, dates_from)),
               cases = ifelse(cases < 0, 0, cases),
               cases = round(cases))

      

# Forecast deaths ---------------------------------------------------------

      
      # Set deaths data and truncate
      deaths_data_weekly_full <- deaths_data %>%
        mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6)),
               date = NULL) %>%
        group_by(state, week) %>%
        summarise(deaths = sum(deaths))
      
      deaths_data_weekly <- deaths_data_weekly_full %>%
        filter(week <= right_truncate_date)
      
      # Join deaths and cases
      cases_deaths <- dplyr::bind_rows(case_data_weekly, case_forecast) %>%
        full_join(deaths_data_weekly, by = c("state", "week")) %>%
        mutate(week = lubridate::ymd(week))
      
      # Forecast deaths (y) using cases
      death_forecast <- cases_deaths %>%
        group_by(state) %>%
        group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, week %in% historical_weeks) %>% pull("deaths"),
                                                     samples = sample_count, 
                                                     horizon = horizon_weeks,
                                                     model_params = list(models = "aefz", weights = "equal",
                                                                         a.args = list(
                                                                         xreg = filter(.x, week %in% historical_weeks) %>% pull("cases"))
                                                                         ),
                                                     forecast_params = list(xreg = filter(.x, week %in% forecast_weeks) %>% pull("cases"),
                                                                            PI.combination = "mean")
                                                     )) %>%
        mutate(sample = rep(1:sample_count)) %>%
        pivot_longer(cols = starts_with("..."), names_to = "week")
      
      

# Return forecast ---------------------------------------------------------

      if(format == FALSE){
        return(death_forecast)
      }
      
      if(format == TRUE){
        # Get quantiles
        quantile <- death_forecast %>%
          group_by(state, week) %>%
          group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
          mutate(quantile = quantiles_out) %>%
          ungroup() %>%
          select(state, week, quantile, "deaths" = 3) %>%
          mutate(week = recode(week, !!! setNames(forecast_weeks, dates_from)),
                 deaths = ifelse(deaths < 0, 0, deaths),
                 deaths = round(deaths),
                 model_type = "deaths_on_cases",
                 date_created = Sys.Date())
        
        return(quantile)
      }
}




      
      

      
      
  