
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
  dplyr::select(-n) 

# store qauntiles available
tau <- full_set$quantile %>%
  round(digits = 3) %>%
  unique()

# load deaths
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

# join deaths with past forecasts and reformat
combined <- full_set %>%
  dplyr::inner_join(epiweek_to_date) %>%
  dplyr::inner_join(deaths) %>%
  tidyr::pivot_wider(values_from = value, names_from = quantile, 
                     names_prefix="quantile_") %>%
  dplyr::arrange(forecast_date, target, target_end_date, location, model, epiweek) %>%
  dplyr::select(-c(forecast_date, target, target_end_date, location, epiweek, state)) 

# extract true values and check if they have the correct length
models <- unique(combined$model)

true_values <- combined %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(n = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(n) %>%
  dplyr::summarise(deaths = unique(deaths)) %>%
  .$deaths

# this should be TRUE
length(true_values) == (nrow(combined))  / length(models)

# extract forecasts as matrices and store as quantgen array
qarr <- combined %>%
  dplyr::select(-deaths) %>%
  dplyr::group_split(model, .keep = FALSE) %>%
  setNames(models) %>%
  purrr::map(.f = as.matrix) %>%
  quantgen::combine_into_array()

model_weights <- quantgen::quantile_ensemble(qarr = qarr, 
                                             y = true_values, 
                                             tau = tau)$alpha


# ensembling -------------------------------------------------------------------
forecasts <- load_submission_files(dates = "latest",
                                   models = c("rt", "deaths-only", "deaths-on-cases"))

# pivot_wider
forecasts_wide <- forecasts %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)
  

qra_ensemble <- forecasts_wide %>%
  dplyr::mutate(ensemble = forecasts_wide %>% 
                dplyr::select(dplyr::all_of(models)) %>%
                as.matrix() %>%
                matrixStats::rowWeightedMeans(w = model_weights, 
                                              na.rm = TRUE)) %>%
  dplyr::rename(value = ensemble) %>%
  dplyr::select(-dplyr::all_of(models)) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value) %>%
  # round values after ensembling
  dplyr::mutate(value = round(value)) 



# write dated file
forecast_date <- Sys.Date()
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", 
                                            "submission-files","dated",
                                    paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# write Latest files
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-qra.csv")))

