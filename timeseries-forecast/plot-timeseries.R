# Plot timeseries forecasts
library(ggplot2); library(dplyr); library(stringr)

# Run update for timeseries forecasts
source(here::here("timeseries-forecast", "update-timeseries.R"))

# Reshape for plotting
plot_state_ts <- state_forecast %>%
  group_by(state, date) %>%
  mutate(quantile = str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  pivot_wider(id_cols = c(state, date), names_from = quantile, values_from = deaths) %>%
  ungroup()

# Plot
plot_state_ts %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_line(data = data_state, aes(y = deaths), col = "blue") +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Daily incident deaths") +
  ggplot2::xlab("Date") +
  ggplot2::geom_vline(xintercept = min(forecast_dates), lty = 2) 
  