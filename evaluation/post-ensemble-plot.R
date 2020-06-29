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
rt_forecasts <- readr::read_csv(here::here("rt-forecast", "submission-files",
                                           "latest-rt-forecast-submission.csv")) %>%
  dplyr::mutate(model = "Rt")

## Get timeseries forecasts
ts_deaths_only <- readr::read_csv(here::here("timeseries-forecast", "deaths-only",
                                             "submission-files",
                                             "latest-weekly-deaths-only.csv")) %>%
  dplyr::mutate(model = "TS deaths")

ts_deaths_on_cases <- readr::read_csv(here::here("timeseries-forecast", "deaths-on-cases",
                                                 "submission-files", 
                                                 "latest-weekly-deaths-on-cases.csv")) %>%
  dplyr::mutate(model = "TS deaths on cases")

## Get QRA ensemble
qra_ensemble <- readr::read_csv(here::here("ensembling", "qra-ensemble",
                                       "submission-files",
                                       "latest-epiforecasts-ensemble1-qra.csv")) %>%
  dplyr::mutate(model = "QRA ensemble")

## Get mean average ensemble
mean_ensemble <- readr::read_csv(here::here("ensembling", "quantile-average",
                                       "submission-files",
                                       "latest-epiforecasts-ensemble1-qa.csv")) %>%
  dplyr::mutate(model = "Mean ensemble")


# Join forecasts ----------------------------------------------------------
# and add state names
forecasts <- dplyr::bind_rows(rt_forecasts, ts_deaths_only, ts_deaths_on_cases, 
                              qra_ensemble, mean_ensemble) %>%
  dplyr::left_join(tigris::fips_codes %>%
              dplyr::select(state_code, state = state_name) %>%
              unique() %>%
              rbind(c("US", "US")),
              by = c("location" = "state_code"))


# Reshape forecasts and add observed data --------------------------------------------------------------------
# Filter to incidence forecasts and pivot forecasts for plotting
forecasts_state <- forecasts %>%
  filter(grepl("inc", target)) %>%
  group_by(state, target_end_date, model) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, target_end_date, model), 
                     names_from = quantile, values_from = value) %>%
  ungroup() 
  
forecasts_national <- forecasts %>%
  filter(state %in% "US",
         grepl("inc", target)) %>%
  group_by(state, target_end_date, model) %>%
  mutate(quantile = stringr::str_c("c", quantile)) %>%
  filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
  tidyr::pivot_wider(id_cols = c(state, target_end_date, model), names_from = quantile, values_from = value) %>%
  ungroup()

# Set observed data to match format
observed_deaths_state <- dplyr::filter(weekly_deaths_state) %>%
  dplyr::mutate(model = "Observed") %>%
  dplyr::select(-epiweek, c0.5 = deaths)

observed_deaths_national <- weekly_deaths_national %>%
  dplyr::mutate(model = "Observed") %>%
  dplyr::select(-epiweek, c0.5 = deaths)

# Identify and filter which states to keep -------------------------------------------

# Identify over 100 cases in the last week
source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 100, last_week = 1)


plot_state <- dplyr::bind_rows(forecasts_state, observed_deaths_state) %>%
  dplyr::filter(state %in% keep_states$state) %>%
  dplyr::mutate(model = factor(model, levels = c("Observed", "Mean ensemble", "QRA ensemble",
                                          "Rt", "TS deaths", "TS deaths on cases")))


plot_national <- bind_rows(forecasts_national, observed_deaths_national) %>%
  mutate(state = "US",
         model = factor(model, 
                        levels = c("Observed", "Mean ensemble", "QRA ensemble", 
                                   "Rt", "TS deaths", "TS deaths on cases")))



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

ggsave(filename = "ensemble-plot-state.png", path = here::here("evaluation"),
      width = 10, height = 6, dpi = 300)


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
