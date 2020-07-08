
library(magrittr)
source(here::here("utils", "load-submissions-function.R"))



# get weights ------------------------------------------------------------------

# load past forecasts
past_forecasts <- load_submission_files(dates = "all",
                                        num_last = 3,
                                        models = c("rt", "deaths-only", "deaths-on-cases")) 


# create complete set

full_set <- past_forecasts %>%
  # remove complete duplicates
  dplyr::distinct() %>%
  # remove point forecasts and cumulative forecasts
  dplyr::filter(type == "quantile", 
                grepl("inc", target)) %>%
  dplyr::select(-type) %>%
  # filter out duplicate predictions for the exact same target quantile
  dplyr::group_by(forecast_date, target, target_end_date, location, quantile, model) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  # remove targets for which not all models have a forecast
  dplyr::group_by(forecast_date, target, target_end_date, location, quantile) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n) %>%
  # hopefully no change from here on, but continue to be sure: 
  # remove all quantiles that are now incomplete
  dplyr::group_by(forecast_date, target, target_end_date, location) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n) %>%
  # remove all locations that are now incomplete
  dplyr::group_by(forecast_date, target, target_end_date) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n) %>%
  # remove all forecast_dates that are now incomplete
  dplyr::group_by(target) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n)
  
# alternative: 
# 
# tidyr::complete(forecast_date, target, target_end_date, location, quantile, model) %>%
#   dplyr::group_by(forecast_date, target, target_end_date, location, quantile) %>%
#   dplyr::mutate(any_na = any(is.na(value))) %>%
#   dplyr::filter(!any_na) %>%
#   dplyr::select(-any_na) %>%
#   dplyr::ungroup()




# ensembling
forecasts <- load_submission_files(dates = "latest",
                                   models = c("rt", "deaths-only", "deaths-on-cases"))


dplyr::add_count(past_forecasts)
