library(tidyverse)

plot_forecasts = function(national = TRUE, states = NULL){
  
  ## Load current observed deaths data
  deaths_data <- readRDS(here::here("rt-forecast/data/deaths_data.rds")) %>%
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
    mutate(type = "observed_data") %>%
    filter(week_beginning < max(week_beginning),
           week_beginning >= as.Date("2020-05-01"))
  
  ## Load most recent forecast data
  forecast_data <- read.csv(here::here("death-forecast/epiforecasts-ensemble1/2020-06-15-epiforecasts-ensemble1.csv")) %>%
    tibble() %>%
    mutate(week = as.Date(target_end_date) - 6) %>%
    left_join(tigris::fips_codes %>%
                dplyr::select(state_code, state_name) %>%
                unique() %>%
                rbind(c("US", "US")),
              by = c("location" = "state_code")) %>%
    select(week_beginning = week, state = state_name, target, type, quantile, value)
  forecast_states = unique(forecast_data$state)
  
  
  # Combine and reshape for plotting
  plotting_data <- forecast_data %>%
    bind_rows(deaths_data %>% filter(state %in% forecast_states)) %>%
    filter(grepl("inc", target) | is.na(target)) %>%
    select(-target) %>%
    mutate(q_type = ifelse(type %in% c("point", "observed_data"), type, paste0(type, quantile))) %>%
    select(week_beginning, state, q_type, value) %>%
    pivot_wider(id_cols = c(week_beginning, state), names_from = q_type, values_from = value)
  
  ##
  if (national) {
    
    plotting_data <- plotting_data %>%
      filter(state %in% "US")
    
  } else if (!national & !is.null(states)) {
    
    plotting_data <- plotting_data %>%
      filter(state %in% states)
    
  } else {
    
    plotting_data <- plotting_data %>%
      filter(state != "US")
    
  }
  
  ## Plotting!
  plotting_data %>%
    ggplot(aes(x = week_beginning)) +
    geom_line(aes(y = observed_data), lwd = 1) +
    geom_point(aes(y = point), size = 1, col = "red") +
    geom_line(aes(y = point), lwd = 1, col = "red") +
    # geom_ribbon(aes(ymin = quantile0.05, ymax = quantile0.95), fill = "red", alpha = 0.2) +
    geom_ribbon(aes(ymin = quantile0.25, ymax = quantile0.75), fill = "red", alpha = 0.2) +
    facet_wrap(.~ state, scales = "free_y") +
    labs(x = "Week beginning", y = "Weekly incident deaths") +
    cowplot::theme_cowplot()
  
}
