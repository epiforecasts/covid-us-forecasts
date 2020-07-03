# Plot timeseries forecasts
library(ggplot2); library(dplyr); library(tidyr); library(stringr)


# Get data and forecasts
source(here::here("utils", "get-us-data.R"))

model_type <- "deaths-only" # or "deaths-on-cases", "log-lin", "log-log"
right_truncate_days = 1

daily_forecast <- readRDS(here::here("timeseries-forecast", "daily", "raw-rds",
                                      paste0("latest-daily-", model_type, ".rds")))

# States ------------------------------------------------------------------

# Get data
daily_deaths_state <- get_us_deaths(data = "daily") 

# Reshape forecast for plotting
fc_state <- daily_forecast %>%
  filter(state %in% keep_states) %>%
  group_by(state, date_target) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, date_target), names_from = quantile, values_from = deaths) %>%
  ungroup()


# Plot
plot_state <- fc_state %>%
  ggplot2::ggplot(ggplot2::aes(x = date_target)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.05)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  ggplot2::geom_line(data = daily_deaths_state, ggplot2::aes(x = date, y = deaths), col = "blue") +
  ggplot2::facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Incident deaths") +
  ggplot2::xlab("") +
  ggplot2::geom_vline(xintercept = max(daily_deaths_state$date, na.rm=T) - right_truncate_days, lty = 2) +
  ggplot2::labs(caption = "--- is date of data truncation",
                title = paste0("Incident deaths in US states, from ", model_type, " model"))
  
  
ggplot2::ggsave(filename = paste0("state-forecast-", model_type, ".png"), plot = plot_state, 
                path = here::here("timeseries-forecast", model_type, "raw-rds"),
                width = 12, height = 6, dpi = 300)

# National ----------------------------------------------------------------

daily_deaths_national <- daily_deaths_state %>%
  mutate(state = "US") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths))
  
# Reshape forecast for plotting
fc_national <- daily_forecast %>%
  filter(state == "US") %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, date_target), names_from = quantile, values_from = deaths) %>%
  ungroup()


# Plot
plot_national <- fc_national %>%
  ggplot2::ggplot(ggplot2::aes(x = date_target)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.95)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  ggplot2::geom_line(data = daily_deaths_national, ggplot2::aes(x = date, y = deaths), col = "blue") +
  ggplot2::facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Incident deaths") +
  ggplot2::xlab("") +
  ggplot2::geom_vline(xintercept = max(daily_deaths_national$date) - right_truncate_days, lty = 2) +
  ggplot2::labs(caption = "--- is date of data truncation",
                title = paste0("Incident deaths in US states, from ", model_type, " model"))

ggplot2::ggsave(filename = paste0("national-forecast-", model_type, ".png"), plot = plot_national,
                path = here::here("timeseries-forecast", model_type, "raw-rds"))

  
