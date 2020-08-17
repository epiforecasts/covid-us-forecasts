
# Load required packages and functions -----------------------------------------

library(magrittr)
source(here::here("utils", "load-submissions-function.R"))

source(here::here("utils", "current-forecast-submission-date.R"))


# Load Forecasts ---------------------------------------------------------------
forecasts <- load_submission_files(dates = "latest",
                                   models = "single")

# get forecast_date
# forecast_date <- Sys.Date()
forecast_date <- max(forecasts$forecast_date)

# average quantiles ------------------------------------------------------------

# store models as strings
models <- unique(forecasts$model)


# pivot_wider
forecasts_wide <- forecasts %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  dplyr::select(-forecast_date) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)

# add average; rename ensemble to value
mean_ensemble <- forecasts_wide %>%
  dplyr::mutate(ensemble = forecasts_wide %>% 
                  dplyr::select(dplyr::all_of(models)) %>%
                  rowMeans()) %>%
  dplyr::rename(value = ensemble) %>%
  dplyr::select(-dplyr::all_of(models)) %>%
  dplyr::select(submission_date, target, target_end_date, location, type, quantile, value) %>%
  # round values after ensembling
  dplyr::mutate(value = round(value),
                forecast_date = forecast_date) 


# store as csv submission ------------------------------------------------------
# Store in QA folder
# Dated
data.table::fwrite(mean_ensemble, here::here("ensembling", "quantile-average", 
                                             "submission-files","dated",
                                             paste0(forecast_date, "-epiforecasts-ensemble1-qa.csv")))
# Latest
data.table::fwrite(mean_ensemble, here::here("ensembling", "quantile-average", "submission-files",
                                             paste0("latest.csv")))
