# Thoughts on time series
#- No account for data truncation - could explore this?
# -- in case forecast / death forecast / both
#- why are both cases and lag case predictors on a rolling 7-day sum?
# -- if to correct for reporting, maybe a rolling MA rather than a sum?


library(magrittr)
library(dplyr)
library(tidyr)
library(EpiSoon)
library(forecastHybrid)
library(lubridate)
library(data.table)

library(ggplot2)


# Set up forecasting functions ------------------------------------------------------------------------
# 1. Original
source(here::here("models", "timeseries", "utils", "deaths-on-cases-forecast.R"))

# 2. Truncated - exclude last 3 days of data from both case and deaths

# 3. Removed rolling sum on cases forecast
deaths_on_cases_forecast_nosum <- function(case_data, deaths_data, sample_count = 1000,
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
    filter(date >= min(deaths_data$date)) %>%
    ungroup() %>%
    drop_na() %>%
    arrange(state) %>%
    full_join(deaths_data, by = c("state", "date")) %>% 
    mutate(forecast = ifelse(is.na(deaths), TRUE, FALSE))
  
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
    mutate(date = max(deaths_data$date) + day - 1, deaths = value) %>%
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
  setorder(samples, state, sample, date)
  return(samples)
}


# 4. Case predictor is moving average instead of sum

# data.table::frollmean

deaths_on_cases_forecast_ma <- function(case_data, deaths_data, sample_count = 1000,
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
      cases = data.table::frollmean(cases, 7, align = "right"),
      cases_lag1 = data.table::frollmean(cases_lag1, 7, align = "right"),
      cases_lag2 = data.table::frollmean(cases_lag2, 7, align = "right")
    ) %>%
    filter(date >= min(deaths_data$date)) %>%
    ungroup() %>%
    drop_na() %>%
    arrange(state) %>%
    full_join(deaths_data, by = c("state", "date")) %>% 
    mutate(forecast = ifelse(is.na(deaths), TRUE, FALSE))
  
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
    mutate(date = max(deaths_data$date) + day - 1, deaths = value) %>%
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
  setorder(samples, state, sample, date)
  return(samples)
}

# 5. Only regression models for death forecast
# = auto.arima [a], nnetar [n], stlm [s]

deaths_on_cases_forecast_regs <- function(case_data, deaths_data, sample_count = 1000,
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
    ungroup() %>%
    drop_na() %>%
    arrange(state) %>%
    full_join(deaths_data, by = c("state", "date")) %>% 
    mutate(forecast = ifelse(is.na(deaths), TRUE, FALSE))
  
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
            models = "as",
            weights = "equal",
            a.args = list(xreg =
                            as.matrix(
                              .x[.x$forecast == FALSE,]$cases,
                              .x[.x$forecast == FALSE,]$cases_lag1,
                              .x[.x$forecast == FALSE,]$cases_lag2
                            )),
            n.args = list(xreg =
                            as.matrix(
                              .x[.x$forecast == FALSE,]$cases,
                              .x[.x$forecast == FALSE,]$cases_lag1,
                              .x[.x$forecast == FALSE,]$cases_lag2
                            )),
            s.args = list(xreg =
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
    mutate(date = max(deaths_data$date) + day - 1, deaths = value) %>%
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
  setorder(samples, state, sample, date)
  return(samples)
}


# 6. Deaths only

deaths_only_forecast <- function(deaths_data, sample_count = 1000,
                                          window = 6, horizon = 30) {
  # Set up observations ----------------------------------------------------------
  
  deaths_data <- deaths_data %>%
    group_by(state) %>%
    filter(date >= max(date) - weeks(window)) %>%
    ungroup()
  
  # Forecast deaths ---------------------------------------------------------
  death_forecast <- suppressMessages(
    deaths_data %>%
      group_by(state) %>%
      group_modify(
        ~ EpiSoon::forecastHybrid_model(
          y = .x$deaths,
          samples = sample_count,
          horizon = horizon,
          model_params = list(
            models = "aez",
            weights = "equal",
            a.args = list()
          ),
          forecast_params = list(PI.combination = "mean")
        )
      ) %>%
    mutate(sample = rep(1:sample_count)) %>%
    tidyr::pivot_longer(cols = starts_with("..."), names_to = "day") %>%
    mutate(day = day %>%
             stringr::str_remove_all("...") %>%
             as.numeric()) %>%
    mutate(date = max(deaths_data$date) + day - 1, 
           deaths = value) %>%
    select(state, sample, date, deaths))
  
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
  setorder(samples, state, sample, date)
  return(samples)
}


# Set up testing --------------------------------------------------------------------
# forecast date ----
forecast_date <- readRDS(here("data", "target_date.rds"))

# Set up data ---
source(here::here("utils", "get-us-data.R"))
source(here::here("utils", "format-forecast-us.R"))

deaths_state <- get_us_deaths(data = "daily") %>% 
  filter(date <= forecast_date)

cases_state <- get_us_cases(data = "daily") %>% 
  filter(date <= forecast_date)


# forecast states ---------------------------------------------------------

# forecast with each variant
original <- deaths_on_cases_forecast(case_data = cases_state, deaths_data = deaths_state)


truncated <- deaths_on_cases_forecast(case_data = dplyr::filter(cases_state, date <= (Sys.Date() - 3)),
                                            deaths_data = dplyr::filter(deaths_state, date <= (Sys.Date() - 3)))

no_rollsum <- deaths_on_cases_forecast_nosum(case_data = cases_state, deaths_data = deaths_state)

rollmean <- deaths_on_cases_forecast_ma(case_data = cases_state, deaths_data = deaths_state)

deaths_only <- deaths_only_forecast(deaths_data = deaths_state) 

# combine into list
weekly_forecasts_list <- list(original, truncated, no_rollsum, rollmean, deaths_only)
names(weekly_forecasts_list) <- c("original",  "truncated", "no_rollsum", "rollmean", "deaths_only")

# format all forecasts and combine
format <- function(raw_forecast) {
  
  forecasts <- as.data.table(raw_forecast)
  
  # Filter to full epiweeks
  forecasts <- dates_to_epiweek(forecasts)
  forecasts <- forecasts[epiweek_full == TRUE]
  forecasts <- forecasts[,  epiweek := lubridate::epiweek(date)]
  
  # Aggregate to weekly incidence
  weekly_forecasts_inc <- forecasts[,.(deaths = sum(deaths, na.rm = TRUE), target_end_date = max(date)), 
                                    by = .(epiweek, state, sample)]
  
  
  shrink_per <- 0
  weekly_forecasts_inc <- weekly_forecasts_inc[order(deaths)][,
                                                              .SD[round(.N * shrink_per, 0):round(.N * (1 - shrink_per), 0)],
                                                              by = .(epiweek, state)]
  # Take quantiles
  weekly_forecasts <- weekly_forecasts_inc[, 
                                           .(value = quantile(deaths, probs = c(0.01, 0.25, 0.5, 0.75, 0.99), na.rm=T),
                                             quantile = c(0.025, 0.25, 0.5, 0.75, 0.975), 
                                             target_end_date = max(target_end_date)), 
                                           by = .(state, epiweek)][order(state, epiweek)]
  
  weekly_forecasts$forecast <- ifelse(weekly_forecasts$target_end_date <= forecast_date, 
                                      FALSE, TRUE)
  
  return(weekly_forecasts)
}

formatted <- purrr::map(weekly_forecasts_list, ~ format(.x)) %>%
  bind_rows(.id = "type")

# plot --------------------------------------------------------------------

typecols <- c("original" = "dark grey",  "truncated" = "orange", 
              "no_rollsum" = "purple", "rollmean" = "blue", 
              "deaths_only" = "red")

formatted %>%
  tidyr::pivot_wider(values_from = value, names_from = quantile) %>%
  ggplot(aes(x = target_end_date, col = type, fill = type)) +
  geom_vline(xintercept = as.Date(forecast_date), lty = 2) +
  geom_point(aes(y = `0.5`)) +
  geom_line(aes(y = `0.5`)) +
  geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), col = NA, alpha = 0.02) +
  geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), col = NA, alpha = 0.05) +
  scale_color_manual(values = typecols) +
  scale_fill_manual(values = typecols) +
  facet_wrap(~ state, scales = "free") +
  cowplot::theme_cowplot() +
  labs(x = "Week ending", y = "Weekly incident deaths",  
       col = "type", fill = "type") +
  theme(legend.position = "bottom") +
  ggsave("timeseries.jpg", height = 18, width = 18)

