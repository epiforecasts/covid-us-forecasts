library(magrittr)
library(dplyr)
library(tidyr)
library(EpiSoon)
library(forecastHybrid)
library(lubridate)
library(data.table)

deaths_on_cases_forecast <- function(case_data, deaths_data, sample_count = 1000,
                                     window = 6, horizon = 30) {
    # Set up observations ----------------------------------------------------------
    case_data <- case_data %>%
      group_by(state) %>%
      filter(date >= max(date) - weeks(window + 3)) %>%
      ungroup() %>% 
      select(state, date, cases)
    
    deaths_data <- deaths_data %>%
      group_by(state) %>%
      filter(date >= max(date) - weeks(window)) %>%
      ungroup()
     
    # Forecast cases ---------------------------------------------------------
    case_forecast <- suppressMessages(
      case_data %>%
        group_by(state) %>%
        group_modify(
          ~ EpiSoon::forecastHybrid_model(
            y = .x$cases,
            samples = 1,
            horizon = horizon,
            model_params = list(
              models = "aez",
              weights = "equal",
              a.args = list()
            ),
            forecast_params = list(PI.combination = "mean")
          )
        ) %>%
        tidyr::pivot_longer(cols = starts_with("X"), names_to = "day")
    ) %>%
      mutate(day = day %>%
               stringr::str_remove_all("X") %>%
               as.numeric()) %>%
      mutate(date = max(case_data$date) + day, cases = value) %>%
      select(state, date, cases)
    
    # Forecast deaths ---------------------------------------------------------
    # Join deaths and cases
    cases_deaths <- dplyr::bind_rows(case_data, case_forecast) %>%
      arrange(date) %>%
      group_by(state) %>%
      mutate(cases_lag1 = dplyr::lag(cases, 7),
             cases_lag2 = dplyr::lag(cases, 14)) %>%
      
      mutate(
        cases = data.table::frollsum(cases, 7),
        cases_lag1 = data.table::frollsum(cases_lag1, 7),
        cases_lag2 = data.table::frollsum(cases_lag2, 7)
      ) %>%
      filter(date >= min(deaths_data$date)) %>%
      mutate(forecast = ifelse(date > max(deaths_data$date), TRUE, FALSE)) %>%
      ungroup() %>%
      drop_na() %>%
      arrange(state) %>%
      full_join(deaths_data, by = c("state", "date"))
    
    # Forecast deaths (y) using cases
    death_forecast <- suppressWarnings(suppressMessages(
      cases_deaths %>%
        group_by(state) %>%
        group_modify(
          ~ EpiSoon::forecastHybrid_model(
            y = .x[.x$forecast == FALSE,]$deaths,
            samples = sample_count,
            horizon = horizon,
            model_params = list(
              models = "aez",
              weights = "equal",
              a.args = list(xreg =
                              as.matrix(
                                .x[.x$forecast == FALSE,]$cases,
                                .x[.x$forecast == FALSE,]$cases_lag1,
                                .x[.x$forecast == FALSE,]$cases_lag2
                              ))
            ),
            forecast_params = list(
              xreg =
                as.matrix(.x[.x$forecast == TRUE,]$cases,
                          .x[.x$forecast == TRUE,]$cases_lag1,
                          .x[.x$forecast == TRUE,]$cases_lag2),
              PI.combination = "mean"
            )
          )
        )
    )) %>%
      mutate(sample = rep(1:sample_count)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "day") %>%
      mutate(day = day %>%
               stringr::str_remove_all("...") %>%
               as.numeric()) %>%
      mutate(date = max(deaths_data$date) + day, deaths = value) %>%
      select(state, sample, date, deaths)
    
# Link in observations ----------------------------------------------------

    obs <- expand_grid(sample = 1:sample_count, data = list(deaths_data))
    obs <- obs %>% 
      unnest(data) %>% 
      select(state, sample, date, deaths)
    
    samples <- obs %>% 
      bind_rows(death_forecast)
    
# Return forecast ---------------------------------------------------------
    samples <- samples %>%
      mutate(deaths = ifelse(deaths < 0, 0, deaths),
             deaths = round(deaths))
    return(samples)
}