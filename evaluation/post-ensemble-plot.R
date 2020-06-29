# Plot timeseries, Rt, ensemble, and observed data

library(magrittr)

# Get observed data ------------------------------------------------------------------
source(here::here("utils", "get-us-data.R"))

daily_deaths_state <- get_us_deaths(data = "daily") %>%
  dplyr::mutate(day = ordered(weekdays(as.Date(date)), 
                       levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                epiweek_day = as.numeric(paste0(epiweek, ".", as.numeric(day))))

weekly_deaths_state <- daily_deaths_state %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
  dplyr::group_by(state, epiweek) %>%
  dplyr::summarise(deaths = sum(deaths),
                   target_end_date = max(date)) %>%
  dplyr::ungroup()

weekly_deaths_national <- weekly_deaths_state %>%
  dplyr::group_by(epiweek, target_end_date) %>%
  dplyr::summarise(deaths = sum(deaths)) %>%
  dplyr::ungroup()

# Get forecasts -----------------------------------------------------------

## Get most recent Rt forecast 
rt_files <- list.files(here::here("rt-forecast", "submission-files"))
latest_rt_file <- sort(rt_files, decreasing = TRUE)[1]
rt_path <- paste("rt-forecast/submission-files/", latest_rt_file, sep = "")

rt_forecasts <- readr::read_csv(rt_path) %>%
  dplyr::mutate(model = "Rt")

## Get timeseries forecasts
ts_deaths_only <- readr::read_csv(here::here("timeseries-forecast", "deaths-only", "latest-weekly-deaths-only.csv")) %>%
  dplyr::mutate(model = "TS deaths")
ts_deaths_on_cases <- readr::read_csv(here::here("timeseries-forecast", "deaths-on-cases", "latest-weekly-deaths-on-cases.csv")) %>%
  dplyr::mutate(model = "TS deaths on cases")

## Get ensemble
ensemble_files <- list.files(here::here("ensembling", "qra-ensemble", "submission-files"))
latest_ensemble <- sort(ensemble_files, decreasing = TRUE)[1]
ensemble_path <- paste("ensembling/qra-ensemble/submission-files/", latest_ensemble, sep = "")

ensemble <- readr::read_csv(ensemble_path) %>%
  dplyr::mutate(model = "Ensemble")

# Join and add state names
forecasts <- dplyr::bind_rows(rt_forecasts, ts_deaths_only, ts_deaths_on_cases, ensemble) %>%
  dplyr::left_join(tigris::fips_codes %>%
              dplyr::select(state_code, state = state_name) %>%
              unique() %>%
              rbind(c("US", "US")),
              by = c("location" = "state_code"))



# # Select which states to keep -------------------------------------------

# over 100 cases in the last week
keep_states <- dplyr::filter(weekly_deaths_state, epiweek == max(epiweek)-1 & deaths > 99) %>%
  dplyr::pull(state)

observed_deaths_state <- dplyr::filter(weekly_deaths_state, state %in% keep_states) %>%
  dplyr::mutate(model = "Observed") %>%
  dplyr::select(-epiweek, c0.5 = deaths)

observed_deaths_national <- weekly_deaths_national %>%
  dplyr::mutate(model = "Observed") %>%
  dplyr::select(-epiweek, c0.5 = deaths)


# Reshape forecasts and add observed data --------------------------------------------------------------------

forecasts_state <- forecasts %>%
  filter(state %in% keep_states,
         grepl("inc", target)) %>%
  group_by(state, target_end_date, model) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, target_end_date, model), names_from = quantile, values_from = value) %>%
  ungroup() 
  
plot_state <- bind_rows(forecasts_state, observed_deaths_state) %>%
  mutate(model = factor(model, levels = c("Observed", "Ensemble", "Rt", "TS deaths", "TS deaths on cases")))



forecasts_national <- forecasts %>%
  filter(state %in% "US",
         grepl("inc", target)) %>%
  group_by(state, target_end_date, model) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, target_end_date, model), names_from = quantile, values_from = value) %>%
  ungroup()

plot_national <- bind_rows(forecasts_national, observed_deaths_national) %>%
  mutate(state = "US",
    model = factor(model, levels = c("Observed", "Ensemble", "Rt", "TS deaths", "TS deaths on cases")))


# Plot --------------------------------------------------------------------
library(ggplot2)
library(RColorBrewer)


plot_state %>%
  ggplot(aes(x = target_end_date, col = model, fill = model)) +
  geom_point(aes(y = c0.5), size = 1) +
  geom_line(aes(y = c0.5), lwd = 1) +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), color = NA, alpha = 0.15) +
  ##
  scale_fill_manual(values = c("grey", brewer.pal(5, name = "Set2"))) +
  scale_color_manual(values = c("dark grey", brewer.pal(5, name = "Set2"))) +
  facet_wrap(.~ state, scales = "free_y") +
  labs(x = "Week ending", y = "Weekly incident deaths",
       col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom")

ggsave(filename = "ensemble-plot-state.png", path = here::here("evaluation"))


plot_national %>%
  ggplot(aes(x = target_end_date, col = model, fill = model)) +
  geom_point(aes(y = c0.5), size = 2) +
  geom_line(aes(y = c0.5), lwd = 1) +
  geom_ribbon(aes(ymin = c0.25, ymax = c0.75), color = NA, alpha = 0.15) +
  ##
  scale_fill_manual(values = c("grey", brewer.pal(4, name = "Set2"))) +
  scale_color_manual(values = c("dark grey", brewer.pal(4, name = "Set2"))) +
  facet_wrap(.~ state, scales = "free_y") +
  labs(x = "Week ending", y = "Weekly incident deaths",
       col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom")

ggsave(filename = "ensemble-plot-national.png", path = here::here("evaluation"))
