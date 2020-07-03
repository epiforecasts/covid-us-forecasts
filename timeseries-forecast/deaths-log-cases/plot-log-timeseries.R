# Plot timeseries forecasts
library(ggplot2); library(dplyr); library(tidyr); library(stringr)



# Get data & forecasts ----------------------------------------------------

source(here::here("utils", "get-us-data.R"))

model_type <- "raw" # or "log-lin" or "log-log"
right_truncate_weeks = 1

weekly_forecast <- readRDS(here::here("timeseries-forecast", "deaths-log-cases",
                                      paste0("latest-weekly-", model_type, ".rds")))


# National data & forecast ----------------------------------------------------------------

daily_deaths_national <- daily_deaths_state %>%
  mutate(state = "US") %>%
  group_by(epiweek) %>%
  summarise(deaths = sum(deaths))

weekly_deaths_national <- daily_deaths_state %>%
  mutate(state = "US",
         epiweek = lubridate::epiweek(date)) %>%
  group_by(epiweek) %>%
  summarise(deaths = sum(deaths))

  
# Reshape forecast for plotting
fc_national <- weekly_forecast %>%
  filter(state == "US") %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, epiweek_target), names_from = quantile, values_from = deaths) %>%
  ungroup()



# Plot --------------------------------------------------------------------

plot_national <- fc_national %>%
  ggplot2::ggplot(ggplot2::aes(x = epiweek_target)) +
  ggplot2::geom_line(ggplot2::aes(y = c0.5)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.05, ymax = c0.95)) +
  ggplot2::geom_ribbon(alpha = 0.2, ggplot2::aes(ymin = c0.25, ymax = c0.75)) +
  ggplot2::geom_line(data = weekly_deaths_national, ggplot2::aes(x = epiweek, y = deaths), col = "blue") +
  ggplot2::geom_line(data = daily_deaths_national, ggplot2::aes(x = epiweek, y = deaths), col = "light blue") +
  ggplot2::facet_wrap("state", scales = "free_y") +
  cowplot::theme_cowplot() +
  ggplot2::ylab("Incident deaths") +
  ggplot2::xlab("Epiweek") +
  ggplot2::geom_vline(xintercept = max(weekly_deaths_national$epiweek) - right_truncate_weeks, lty = 2) +
  ggplot2::labs(caption = "--- is date of data truncation",
                title = paste0("Incident deaths in US states, from ", model_type, " model"))

# Save
ggplot2::ggsave(filename = paste0("national-forecast-", model_type, ".png"), plot = plot_national, 
                path = here::here("timeseries-forecast", "deaths-log-cases"))
  
