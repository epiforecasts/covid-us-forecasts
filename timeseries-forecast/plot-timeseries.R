# Plot timeseries forecasts
library(ggplot2); library(dplyr); library(tidyr); library(stringr)

# Run update for timeseries forecasts
source(here::here("timeseries-forecast", "update-timeseries.R"))
rm(list=setdiff(ls(), c("cases_national", 
                        "cases_state",
                        "forecast_dates",
                        "daily_forecast_deaths_only",
                        "incident_deaths_only",
                        "cumulative_deaths_only",
                        "daily_forecast_deaths_on_cases",
                        "incident_deaths_on_cases",
                        "cumulative_deaths_on_cases")))



# Reshape for plotting
plot_ts <- deaths_on_cases_forecast %>%
  mutate(week = date) %>%
  group_by(state, week) %>%
  mutate(quantile = str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  pivot_wider(id_cols = c(state, week), names_from = quantile, values_from = deaths) %>%
  ungroup()

weekly_deaths <- bind_rows(deaths_state, deaths_national) %>%
  mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6))) %>%
  group_by(state, week) %>%
  summarise(deaths = sum(deaths)) %>%
  filter(week < max(week))


# Plot
plot_ts %>%
  ggplot2::ggplot(ggplot2::aes(x = week)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_line(data = weekly_deaths, aes(y = deaths), col = "blue") +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Weekly incident deaths") +
  ggplot2::xlab("Date") #+
  #ggplot2::geom_vline(xintercept = min(forecast_dates), lty = 2) 
  


