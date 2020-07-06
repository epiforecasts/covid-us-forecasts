# ============================================================================ #
# load data
# ============================================================================ #
library(magrittr)


# function for loading past submissions
source(here::here("utils", "load-submission-files.R"))

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
  dplyr::mutate(range = round(range, digits = 0)) %>%
  dplyr::filter(range %in% c(0, 25, 50, 90)) %>%
  dplyr::select(-target, -target_end_date, -quantile)


# ============================================================================ #
# evaluate forecasts
# ============================================================================ #

scores <- scoringutils::eval_forecasts(full, summarise = FALSE)

plots <- scoringutils::plot_scores(scores, facet_formula = ~ range)

plots$bias_plot


# # also add median and iqr and ci
# # this is done to use pre-existing plotting functions
# missing_scores <- df %>%
#   dplyr::group_by(forecast_date, id, region, model) %>%
#   dplyr::summarise(median = median(predictions), 
#                    iqr = IQR(predictions), 
#                    ci = scoringutils::interval_score(true_values = predictions, 
#                                                      lower = quantile(predictions, prop = 0.025), 
#                                                      upper = quantile(predictions, prop = 0.975), 
#                                                      95))
# 
# # join together to obtain all scores and forecasts
# full <- dplyr::inner_join(scored_forecasts, missing_scores) %>%
#   dplyr::rename(dss = DSS, 
#                 crps = CRPS, 
#                 calibration = pit_p_val) %>%
#   dplyr::mutate(logs = NA)
# 
# # summarise scores
# summarised_scores <- EpiSoon::summarise_scores(full)



# ============================================================================ #
# plot evaluation
# ============================================================================ #

# use pre-existing functions


