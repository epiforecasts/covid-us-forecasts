# QRA per region
# 
# Arguments
# past_forecasts = past forecasts for all models
# latest_forecasts = latest forecasts for all models
# deaths_data = observed data
# state_name = name of state to ensemble

regional_qra <- function(past_forecasts, latest_forecasts, deaths_data, 
                         state_name = NULL, return_weights = FALSE){
  
  print(state_name)
  
  past_forecasts <- dplyr::filter(past_forecasts, state == state_name)
  deaths_data <- dplyr::filter(deaths_data, state == state_name)
  latest_forecasts <- dplyr::filter(latest_forecasts, state == state_name)
  
  ### Create complete set
  full_set <- past_forecasts %>%
    dplyr::select(-forecast_date) %>%
    dplyr::filter(type == "quantile", 
                  grepl("inc", target)) %>%
    dplyr::select(-type) %>%
    dplyr::group_by(submission_date, target, target_end_date, location, quantile) %>%
    dplyr::add_count() %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::select(-n) 
  
  
  # store quantiles available
  tau <- full_set$quantile %>%
    round(digits = 3) %>%
    unique()
  
  ### Do the QRA
  
  # join deaths with past forecasts and reformat
  combined <- full_set %>%
    dplyr::mutate(epiweek = lubridate::epiweek(target_end_date)) %>%
    dplyr::inner_join(deaths_data, by = c("state", "epiweek")) %>%
    tidyr::pivot_wider(values_from = value, names_from = quantile, 
                       names_prefix="quantile_") %>%
    dplyr::arrange(submission_date, target, target_end_date, location, model, epiweek) %>%
    dplyr::select(-c(submission_date, target, target_end_date, location, epiweek, state)) 
  
  # extract true values and check if they have the correct length
  models <- unique(combined$model)
  
  true_values <- combined %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(n = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(deaths = unique(deaths), .groups = "drop_last") %>%
    .$deaths
  
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
  
  if(return_weights){
    weights <- tibble::tibble(model = models, weight = model_weights, state = state_name)
    return(weights)
  }
  
  # Quick fix for where states have lost a model in the latest forecast
  model_weights <- ifelse(model_weights < 0, 0, model_weights)
  
  names(model_weights) <- models
  
  latest_models <- unique(latest_forecasts$model)
  
  if(length(latest_models) != length(model_weights)) {
    models <- models[names(model_weights) %in% latest_models]
    model_weights <- model_weights[names(model_weights) %in% latest_models]
  }
  
  ### Ensemble current submission data
  # pivot_wider
  forecasts_wide <- latest_forecasts %>%
    dplyr::select(-forecast_date) %>%
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
    dplyr::select(-dplyr::any_of(models)) %>%
    dplyr::mutate(forecast_date = Sys.Date()) %>%
    dplyr::select(forecast_date, submission_date, target, target_end_date, location, type, quantile, value) %>%
    # round values after ensembling
    dplyr::mutate(value = round(value),
                  state = state_name) 
  
  return(qra_ensemble)
}



