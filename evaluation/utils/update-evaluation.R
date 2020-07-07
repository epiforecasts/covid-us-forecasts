# ============================================================================ #
# load data
# ============================================================================ #
library(magrittr)

# function for loading past submissions
source(here::here("utils", "load-submissions-function.R"))

# function for loading past data
source(here::here("utils", "get-us-data.R"))

past_forecasts <- load_submission_files(dates = "all") %>%
  dplyr::filter(grepl("inc", target), 
                type == "quantile")

deaths <- get_us_deaths(data = "daily") %>%
  dplyr::group_by(epiweek, state) %>%
  dplyr::summarise(deaths = sum(deaths))

# code to get from epiweek to target date copied from Kath
epiweek_to_date <- tibble::tibble(date = seq.Date(from = (as.Date("2020-01-01")), 
                                                  by = 1, length.out = 365)) %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date),
                day = weekdays(date)) %>%
  dplyr::filter(day == "Saturday") %>%
  dplyr::select(target_end_date = date, epiweek)

# join and reformat for scoring
combined <- past_forecasts %>%
  dplyr::inner_join(epiweek_to_date) %>%
  dplyr::inner_join(deaths) %>%
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
                horizon = as.numeric(substring(target, 1, 1))) %>%
  dplyr::filter(range %in% c(0, 20, 50, 90)) %>%
  dplyr::select(-target, -target_end_date, -quantile, -type, -location)


# ============================================================================ #
# evaluate forecasts and plot
# ============================================================================ #

scores <- scoringutils::eval_forecasts(full, summarise = TRUE, 
                                       by = c("model", "state", 
                                              "horizon"))


# source plotting function
source(here::here("evaluation", "utils", "evaluation-plots-function.R"))

plots <- plot_scores(scores)

current_date <- Sys.Date()

if(!dir.exists(here::here("evaluation", "plots", 
                          current_date))) {
  dir.create(here::here("evaluation", "plots", 
                        current_date))
}

ggplot2::ggsave(here::here("evaluation", "plots", 
                  current_date, "interval_scores.png"), 
       plot = plots$interval_score_plot, 
       width = 10, height = 10, dpi = 300)

ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "calibration.png"), 
                plot = plots$calibration_plot, 
                width = 10, height = 10, dpi = 300)

ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "bias.png"), 
                plot = plots$bias_plot, 
                width = 10, height = 10, dpi = 300)

ggplot2::ggsave(here::here("evaluation", "plots", 
                           current_date, "sharpness.png"), 
                plot = plots$sharpness_plot, 
                width = 10, height = 10, dpi = 300)


