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
library(lubridate);

deaths_on_cases_forecast <- function(case_data, deaths_data, case_quantile,
                                     sample_count, window = 6, horizon = 30){
# Set up observations ----------------------------------------------------------
  case_data <- case_data %>% 
    group_by(state) %>% 
    filter(date >= max(date) - weeks(window + 3)) %>% 
    ungroup()
  
  deaths_data <- deaths_data %>% 
    group_by(state) %>% 
    filter(date >= max(date) - weeks(window)) %>% 
    ungroup()
  
# Forecast cases ---------------------------------------------------------
  case_forecast <- suppressMessages(case_data %>%
      group_by(state) %>%
      group_modify(~ EpiSoon::forecastHybrid_model(y = .x$cases,
                                                   samples = 1, 
                                                   horizon = horizon,
                                                   model_params = list(models = "aez", weights = "equal",
                                                                       a.args = list()),
                                                   forecast_params = list(PI.combination = "mean"))) %>%
      tidyr::pivot_longer(cols = starts_with("X"), names_to = "day")) %>% 
      mutate(day = day %>% 
               stringr::str_remove_all("X") %>% 
               as.numeric()) %>% 
      mutate(date = max(case_data$date) + day, cases = value) %>% 
      select(state, date, cases)

# Forecast deaths ---------------------------------------------------------
      
      # Join deaths and cases
      cases_deaths <- dplyr::bind_rows(case_data %>% 
                                         select(-epiweek), case_forecast) %>%
        arrange(date) %>% 
        group_by(state) %>%
        mutate(cases_lag1 = dplyr::lag(cases, 7),
               cases_lag2 = dplyr::lag(cases, 14)) %>%
    
        mutate(cases = data.table::frollsum(cases, 7),
               cases_lag1 = data.table::frollsum(cases_lag1, 7),
               cases_lag2 = data.table::frollsum(cases_lag2, 7)) %>%
        filter(date >= min(deaths_data$date)) %>% 
        ungroup() %>%
        drop_na() %>% 
        arrange(state) %>% 
        full_join(deaths_data, by = c("state", "date"))
      
      # Forecast deaths (y) using cases
      death_forecast <- suppressMessages(cases_deaths %>%
        group_by(state) %>%
        group_modify(~ EpiSoon::forecastHybrid_model(y = .x$deaths,
                                                     samples = sample_count, 
                                                     horizon = horizon,
                                                     model_params = list(models = "aez", weights = "equal",
                                                                         a.args = list(
                                                                         xreg = 
                                                                         as.matrix(
                                                                           .x$cases,
                                                                           .x$cases_lag1,
                                                                           .x$cases_lag2)
                                                                         )),
                                                     forecast_params = list(xreg = 
                                                                              as.matrix(
                                                                                .x$cases,
                                                                                .x$cases_lag1,
                                                                                .x$cases_lag2),
                                                                            PI.combination = "mean"))
                                                     )) %>%
        mutate(sample = rep(1:sample_count)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "day")

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




      
      

      
      
  