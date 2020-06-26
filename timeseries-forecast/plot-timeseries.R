# Plot timeseries forecasts
library(ggplot2); library(dplyr); library(tidyr); library(stringr)


# Get data and forecasts
source(here::here("utils", "get_us_data.R"))

model_type <- "deaths-only"
right_truncate_weeks = 1

weekly_forecast <- readRDS(here::here("timeseries-forecast", model_type,
                                      paste0("latest-weekly-", model_type, ".rds")))


# States ------------------------------------------------------------------

# Get data
daily_deaths_state <- get_us_deaths(data = "daily")

weekly_deaths_state <- daily_deaths_state %>%
  mutate(week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6)),
         date = NULL) %>%
  group_by(state, week) %>%
  summarise(deaths = sum(deaths))

keep_states <- filter(weekly_deaths_state, week == max(week)-7 & deaths > 99) %>%
  pull(state)

weekly_deaths_state <- filter(weekly_deaths_state, state %in% keep_states)
daily_deaths_state <- filter(daily_deaths_state, state %in% keep_states)

# Reshape forecast for plotting
fc_state <- weekly_forecast %>%
  filter(state %in% keep_states) %>%
  group_by(state, week) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, week), names_from = quantile, values_from = deaths) %>%
  ungroup()


# Plot
plot_state <- fc_state %>%
  ggplot2::ggplot(ggplot2::aes(x = week)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_line(data = weekly_deaths_state, ggplot2::aes(y = deaths), col = "blue") +
  ggplot2::geom_line(data = daily_deaths_state, ggplot2::aes(x = date, y = deaths), col = "light blue") +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  ggplot2::facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Weekly incident deaths") +
  ggplot2::xlab("Week") +
  ggplot2::geom_vline(xintercept = max(weekly_deaths_state$week, na.rm=T) - (7 * right_truncate_weeks), lty = 2) +
  ggplot2::labs(caption = "--- is date of data truncation",
                title = paste0("Incident deaths in US states, from ", model_type, " model"))
  
  
ggplot2::ggsave(filename = paste0("state-forecast-", model_type, ".png"), plot = plot_state, 
                path = here::here("timeseries-forecast", model_type),
                width = 12, height = 6, dpi = 300)

# National ----------------------------------------------------------------

weekly_deaths_national <- deaths_state %>%
  mutate(state = "US",
         week = as.Date(lubridate::floor_date(date, unit = "week", week_start = 6))) %>%
  group_by(week) %>%
  summarise(deaths = sum(deaths))
  
# Reshape forecast for plotting
fc_national <- weekly_forecast %>%
  filter(state == "US") %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, week), names_from = quantile, values_from = deaths) %>%
  ungroup()


# Plot
plot_national <- fc_national %>%
  ggplot2::ggplot(ggplot2::aes(x = week)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_line(data = weekly_deaths_national, ggplot2::aes(y = deaths), col = "blue") +
  ggplot2::geom_line(data = deaths_national, ggplot2::aes(x = date, y = deaths), col = "light blue") +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  ggplot2::facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Weekly incident deaths") +
  ggplot2::xlab("Week") +
  ggplot2::geom_vline(xintercept = max(weekly_deaths_national$week) - (7 * right_truncate_weeks), lty = 2) +
  ggplot2::labs(caption = "--- is date of data truncation",
                title = paste0("Incident deaths in US states, from ", model_type, " model"))

ggplot2::ggsave(filename = paste0("national-forecast-", model_type, ".png"), plot = plot_national, 
                path = here::here("timeseries-forecast", model_type))
  
