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
# horizon_days = 7
# models = "az"
# case_quantile = 0.5
# quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)

ts_deaths_on_cases_forecast <- function(case_data, deaths_data, case_quantile,
                                           sample_count, horizon_days, models,
                                           format = FALSE, quantiles_out = NULL){
    

# Forecast cases ----------------------------------------------------------

    # Forecast
    case_forecast <- case_data %>%
      group_by(state) %>%
      group_modify(~ EpiSoon::forecastHybrid_model(y = .x$cases,
                                                   samples = sample_count, 
                                                   horizon = horizon_days,
                                                   model_params = list(models = models))) %>%
      mutate(sample = rep(1:sample_count)) %>%
      pivot_longer(cols = starts_with("..."), names_to = "date")
    
    # Get quantile
      quantile <- case_forecast %>%
        group_by(state, date) %>%
        group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile))) %>%
        mutate(quantile = case_quantile) %>%
        ungroup()
      
      # Set up dates
      dates_from <- unique(quantile$date)
      forecast_dates <- seq(max(case_data$date)+1, by = 1, to = as.Date(max(case_data$date)+horizon_days))
      
      # Format
      case_forecast <- quantile %>%
        select(state, date, "cases" = 3) %>%
        mutate(date = recode(date, !!! setNames(forecast_dates, dates_from)),
               cases = ifelse(cases < 0, 0, cases),
               cases = round(cases))

      

# Forecast deaths ---------------------------------------------------------

      # Join deaths and cases
      cases_deaths <- dplyr::bind_rows(case_data, case_forecast) %>%
        full_join(deaths_data, case_join, by = c("state", "date"))
      
      # Forecast deaths using cases
      death_forecast <- cases_deaths %>%
        group_by(state) %>%
        group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, !date %in% forecast_dates) %>% pull("deaths"),
                                                     samples = sample_count, 
                                                     horizon = horizon_days,
                                                     model_params = list(models = models,
                                                                         a.args = list(
                                                                           xreg = filter(.x, !date %in% forecast_dates) %>% pull("cases")
                                                                         )),
                                                     forecast_params = list(
                                                       xreg = filter(.x, date %in% forecast_dates) %>% pull("cases")
                                                     ))) %>%
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
          group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out))) %>%
          mutate(quantile = quantiles_out) %>%
          ungroup() %>%
          select(state, date, quantile, "deaths" = 3) %>%
          mutate(date = recode(date, !!! setNames(forecast_dates, dates_from)),
                 deaths = ifelse(deaths < 0, 0, deaths),
                 deaths = round(deaths),
                 model_type = "deaths_on_cases",
                 date_created = Sys.Date())
        
        return(quantile)
      }
}




      
      

      
      
  