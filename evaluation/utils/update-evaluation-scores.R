# ============================================================================ #
# load data
# ============================================================================ #
library(magrittr)
library(dplyr)

# function for loading past submissions
source(here::here("utils", "load-submissions-function.R"))

# function for loading past data
source(here::here("utils", "get-us-data.R"))

# load past forecasts
past_forecasts <- load_submission_files(dates = "all") %>%
  dplyr::filter(grepl("inc", target), 
                type == "quantile")

deaths <- get_us_deaths(data = "daily") %>%
  dplyr::group_by(epiweek, state) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last")

# code to get from epiweek to target date copied from Kath
epiweek_to_date <- tibble::tibble(date = seq.Date(from = (as.Date("2020-01-01")), 
                                                  by = 1, length.out = 365)) %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date),
                day = weekdays(date)) %>%
  dplyr::filter(day == "Saturday") %>%
  dplyr::select(target_end_date = date, epiweek)

# join and reformat for scoring
combined <- past_forecasts %>%
  dplyr::inner_join(epiweek_to_date, by = "target_end_date") %>%
  dplyr::inner_join(deaths, by = c("state", "epiweek")) %>%
  dplyr::rename(true_values = deaths, 
                predictions = value) %>%
  dplyr::mutate(boundary = ifelse(quantile <= 0.5, "lower", "upper"), 
                range = abs(1 - 2 * quantile) * 100)
  
full <- dplyr::bind_rows(combined, 
                         combined %>%
                           dplyr::filter(quantile == 0.5) %>%
                           dplyr::mutate(boundary = "upper")) %>%
  # weird unexplicable rounding error?
  dplyr::mutate(range = round(range, digits = 0), 
                horizon = as.numeric(substring(target, 1, 2))) %>%
  dplyr::filter(range %in% c(0, 20, 50, 90)) %>%
  dplyr::select(-target, -target_end_date, -quantile, -type, -location) %>%
  unique() %>%
  # filter away duplicate forecasts if there are any (if there aren't, this has no effect)
  dplyr::group_by(forecast_date, model, state, epiweek, boundary, range, horizon) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()


# ============================================================================ #
# evaluate forecasts and plot
# ============================================================================ #

# source plotting function
source(here::here("evaluation", "utils", "evaluation-plots-function.R"))


# regular plots
scores <- scoringutils::eval_forecasts(full, summarised = TRUE, 
                                       by = c("model", "state", 
                                              "horizon"))

plots <- plot_scores(scores, by = c("model", "range", "horizon"))

# state plots
scores_state <- scoringutils::eval_forecasts(full, summarised = TRUE, 
                                             by = c("model", "state", 
                                                    "horizon"))

plots_state <- plot_scores(scores_state, by = c("model", "range", "state"), 
                           y = "state", 
                           dodge_width = 0.75)


current_date <- Sys.Date()

if(!dir.exists(here::here("evaluation", "plots", 
                          current_date, "scoring"))) {
  dir.create(here::here("evaluation", "plots", 
                        current_date, "scoring"))
}

# regular plots
suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                  current_date, "scoring", "interval_scores.png"), 
       plot = plots$interval_score_plot, 
       width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "calibration.png"), 
                plot = plots$calibration_plot, 
                width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date,"scoring",  "bias.png"), 
                plot = plots$bias_plot, 
                width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "sharpness.png"), 
                plot = plots$sharpness_plot, 
                width = 10, height = 10, dpi = 300))


# state plots
suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "state_interval_scores.png"), 
                plot = plots_state$interval_score_plot, 
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "state_calibration.png"), 
                plot = plots_state$calibration_plot, 
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "state_bias.png"), 
                plot = plots_state$bias_plot, 
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "scoring", "state_sharpness.png"), 
                plot = plots_state$sharpness_plot, 
                width = 10, height = 35, dpi = 300))

