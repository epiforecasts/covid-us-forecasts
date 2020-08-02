
library(magrittr)
source(here::here("utils", "load-submissions-function.R"))



# get weights ------------------------------------------------------------------

# load past forecasts
past_forecasts <- load_submission_files(dates = "all",
                                        num_last = 4, # 2 to include Epinow2-Rt
                                        models = c("rt-2", "rt-1", "deaths-only", "deaths-on-cases")) 

# create complete set
## Note: code to remove duplicates has been commented out. 
## If duplicates exist this is likely to be a near-duplicate forecast,
## that was made but updated the same/next day.
## The model producer should move these near-duplicate forecasts  
## to the relevant model's "out-of-date" folder - before ensembling.

full_set <- past_forecasts %>%
  # remove complete duplicates
  # dplyr::distinct() %>%
  # remove point forecasts and cumulative forecasts
  dplyr::filter(type == "quantile", 
                grepl("inc", target)) %>%
  dplyr::select(-type) %>%
  # filter out duplicate predictions for the exact same target quantile
  # dplyr::group_by(forecast_date, target, target_end_date, location, quantile, model) %>%
  # dplyr::slice(1) %>%
  # dplyr::ungroup() %>%
  # remove targets for which not all models have a forecast
  dplyr::group_by(submission_date, target, target_end_date, location, quantile) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n) 

# store quantiles available
tau <- full_set$quantile %>%
  round(digits = 3) %>%
  unique()

# load deaths
source(here::here("utils", "get-us-data.R"))
deaths <- get_us_deaths(data = "daily") %>%
  dplyr::group_by(epiweek, state) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last")


# join deaths with past forecasts and reformat
combined <- full_set %>%
  dplyr::mutate(epiweek = lubridate::epiweek(target_end_date)) %>%
  dplyr::inner_join(deaths, by = c("state", "epiweek")) %>%
  tidyr::pivot_wider(values_from = value, names_from = quantile, 
                     names_prefix="quantile_") %>%
  dplyr::arrange(submission_date, target, target_end_date, location, model, epiweek) %>%
  dplyr::select(-c(submission_date, forecast_date, target, target_end_date, location, epiweek, state)) 

# extract true values and check if they have the correct length
models <- unique(combined$model)

true_values <- combined %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(n = 1:dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(n) %>%
  dplyr::summarise(deaths = unique(deaths), .groups = "drop_last") %>%
  .$deaths

# this should be TRUE
if(!(length(true_values) == (nrow(combined))  / length(models))){
  warning("QRA: check that true values and models align")
}

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

message("QRA weights:")
message(paste0("\n", models, "\n", model_weights, "\n"))

# ensembling -------------------------------------------------------------------
forecasts <- load_submission_files(dates = "latest",
                                   models = c("rt-2", "rt-1", "deaths-only", "deaths-on-cases"))

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
  dplyr::select(forecast_date, submission_date, target, target_end_date, location, type, quantile, value) %>%
  # round values after ensembling
  dplyr::mutate(value = round(value)) 



# write dated file
forecast_date <- Sys.Date()
# forecast_date <- "2020-07-26"
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", 
                                            "submission-files","dated",
                                    paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# write Latest files
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-qra.csv")))

