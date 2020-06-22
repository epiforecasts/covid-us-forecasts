library(tidyverse)

plot_forecasts = function(national = TRUE, states = NULL, forecast_date = Sys.Date(), cutoff = 25){
  
  forecast_date <- lubridate::floor_date(forecast_date, unit = "week", week_start = 1)

  ## Load current observed deaths data
  # source("utils/get_us_deaths.R")
  deaths_data <- readRDS(here::here("data/deaths_data.rds")) %>%
    dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7)) %>%
    dplyr::group_by(state, week) %>%
    dplyr::summarise(week_deaths = sum(deaths, na.rm = TRUE))
  # Add national total
  deaths_data <- deaths_data %>%
    bind_rows(deaths_data %>%
                dplyr::group_by(week) %>%
                dplyr::summarise(week_deaths = sum(week_deaths, na.rm = TRUE)) %>%
                dplyr::mutate(state = "US")) %>%
    select(week_beginning = week, state, value = week_deaths) %>%
    mutate(type = "observed_data",
           model = "observed_data") %>%
    filter(week_beginning < forecast_date-1,
           week_beginning >= as.Date("2020-05-01"))
  
  high_states <- deaths_data %>%
    dplyr::filter(week_beginning == lubridate::floor_date(forecast_date, unit = "week", week_start = 7)-7) %>%
    filter(value > cutoff,
           state != "US") %>%
    .$state
  
  ## Load most recent Rt forecast data
  forecast_file <- paste0("death-forecast/epiforecasts-ensemble1/", forecast_date, "-epiforecasts-ensemble1.csv")
  forecast_data <- read.csv(here::here(forecast_file)) %>%
    tibble() %>%
    mutate(week = as.Date(target_end_date) - 6) %>%
    left_join(tigris::fips_codes %>%
                dplyr::select(state_code, state_name) %>%
                unique() %>%
                rbind(c("US", "US")),
              by = c("location" = "state_code")) %>%
    select(week_beginning = week, state = state_name, target, type, quantile, value) %>%
    mutate(model = "rt_forecast")
  forecast_states = unique(forecast_data$state)
  
  ## Load most recent deaths time-series forecast
  # ts_deaths_file <- paste0("timeseries-forecast/deaths-only/", "2020-06-22", ".rds")
  ts_deaths_file <- here::here("timeseries-forecast/deaths-only/latest-weekly-inc-deaths-only.csv")
  ts_deaths <- read.csv(ts_deaths_file) %>%
    select(week_beginning = week, state, type = model_type, quantile, value = deaths) %>%
    mutate(week_beginning = as.Date(week_beginning),
           model = "timeseries_deaths_only",
           type = "quantile") %>%
    filter(week_beginning <= max(forecast_data$week_beginning))
  ts_cases_file <- here::here("timeseries-forecast/deaths-on-cases/latest-weekly-inc-deaths-on-cases.csv")
  ts_cases <- read.csv(ts_cases_file) %>%
    select(week_beginning = week, state, type = model_type, quantile, value = deaths) %>%
    mutate(week_beginning = as.Date(week_beginning),
           model = "timeseries_deaths_cases",
           type = "quantile") %>%
    filter(week_beginning <= max(forecast_data$week_beginning))
  
  
  # Combine and reshape for plotting
  plotting_data <- forecast_data %>%
    bind_rows(deaths_data %>% filter(state %in% forecast_states)) %>%
    bind_rows(ts_deaths %>% filter(state %in% forecast_states)) %>%
    bind_rows(ts_cases %>% filter(state %in% forecast_states)) %>%
    filter(grepl("inc", target) | is.na(target)) %>%
    select(-target) %>%
    mutate(q_type = ifelse(type %in% c("point", "observed_data"), type, paste0(type, quantile))) %>%
    select(week_beginning, state, model, q_type, value) %>%
    pivot_wider(id_cols = c(week_beginning, state, model), names_from = q_type, values_from = value)
  
  ##
  if (national) {
    
    plotting_data <- plotting_data %>%
      filter(state %in% "US")
    
  } else if (!national & !is.null(states)) {
    
    plotting_data <- plotting_data %>%
      filter(state %in% states,
             state %in% high_states)
    
  } else {
    
    plotting_data <- plotting_data %>%
      filter(state != "US",
             state %in% high_states)
    
  }
  
  ## Plotting!
  plotting_data %>%
    ggplot(aes(x = week_beginning,  col = model, fill = model)) +
    ## Observed data
    geom_point(aes(y = observed_data), size = 2) +
    geom_line(aes(y = observed_data), lwd = 1) +
    ## Forecasts (50% CI)
    geom_point(aes(y = quantile0.5), size = 2) +
    geom_line(aes(y = quantile0.5), lwd = 1) +
    geom_ribbon(aes(ymin = quantile0.25, ymax = quantile0.75), color = NA, alpha = 0.15) +
    ##
    scale_fill_manual(values = c("white", brewer.pal(3, name = "Set2"))) +
    scale_color_manual(values = c("black", brewer.pal(3, name = "Set2"))) +
    facet_wrap(.~ state, scales = "free_y") +
    labs(x = "Week beginning", y = "Weekly incident deaths",
         col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    theme(legend.position = "top")
  
}
