#' Extract Ensemble Training Data
#'
#' @param forecasts 
#' @param obs
#' @param target_date 
#' @param train_window 
#' @param train_horizons
#' @import data.table
#' @import quantgen
#' @import purrr
#' @import weeks
#' @return
#' @export
extract_training_data <- function(forecasts, obs, target_date, train_window, train_horizons) {
  # null variables to stop global error
  ... <- NULL;
   
  # get time window to train on
  forecasts <- as.data.table(forecasts)
  train_forecasts <- forecasts[forecast_date < target_date & forecast_date >= (target_date - weeks(train_window))]
  # get horizons of interest
  target_horizons <- paste(train_horizons, collapse = " | ")
  train_forecasts <- train_forecasts[grepl(target_horizons, target)]
   
  # checks 
  if (nrow(train_forecasts) == 0) {
    stop("No training data")
  }
  
  # munge 
  train_forecasts <- train_forecasts[, c("submission_date", "target", "type") := NULL]
  train_forecasts <- train_forecasts[!is.na(quantile)]
  train_forecasts <- dcast(train_forecasts, ... ~ quantile, value.var = "value")
  train_forecasts <- merge(train_forecasts, obs, by = c("location", "target_end_date"), all = FALSE)
  
  # only train where all forecasts are present
  train_forecasts <- train_forecasts[, `:=`(model_no = 1:.N, models = .N),
                                     by = .(forecast_date, target_end_date, location)]
  train_forecasts <- train_forecasts[models == max(models)][, models := NULL]
  
  # make quantgen observations format
  true_values <- train_forecasts[model_no == 1]$value
  
  # make quantgen training format
  train_array <- copy(train_forecasts)[, c("forecast_date", "target_end_date", "location", "value",
                                           "model_no") := NULL]
  train_array <- split(train_array, by = "model", keep.by = FALSE)
  tau <- as.numeric(colnames(train_array[[1]]))
  train_array <- map(train_array, as.matrix)
  train_array <- combine_into_array(train_array)
  
  # return output
  out <- list()
  out$true <- true_values
  out$train <- train_forecasts
  out$train_array <- train_array
  out$models <- unique(train_forecasts$model)
  out$tau <- tau
  return(out)
}

#' Fit and Forecast with a quantgen QRA ensemble
#'
#' @param name 
#' @param train 
#' @param true 
#' @param quantiles 
#' @param fit_args 
#' @param models
#' @param forecasts 
#' @import data.table
#' @import quantgen
#' @import stringr 
#' @return
#' @export
ensemble <- function(name, train, true, quantiles, fit_args = NULL, 
                     models, forecasts) {
  # checks 
  if (nrow(forecasts) == 0) {
    stop("No forecasting data")
  }
  # fit QRA
  fit <- do.call(quantile_ensemble, 
                 c(list(qarr = train, y = true, tau = quantiles), fit_args))
  # extract weights
  weights <- data.table(ensemble = name, model = models, weight = round(fit$alpha, 3))
  
  # allocate weights depending on if by quantile or not
  if (length(fit$alpha) == length(models)) {
    weights <- weights[, quantile := list(quantiles)]
    weights <- weights[, .(quantile = unlist(quantile)), by = setdiff(colnames(weights), "quantile")]
  }else{
    weights <- melt(weights, id.vars = c("ensemble", "model"), variable.name = "quantile",
                    value.name = "weight")
    weights <- weights[, quantile := as.numeric(str_remove_all(quantile, "weight."))]
  }
  
  #add NA (i.e point) and order
  weights <- rbindlist(list(
    weights, 
    copy(weights)[quantile == 0.5][, quantile := NA]
  ))
  setorder(weights, ensemble, quantile, model)
  
  # make ensemble forecast
  forecast <- copy(forecasts)[weights, on = c("quantile", "model")]
  forecast <- forecast[, value := value * weight]
  forecast <- forecast[, .(value = sum(value)), by = setdiff(colnames(forecast), c("model", "value", "weight"))]
  forecast <- forecast[, model := ensemble][, ensemble := NULL]
  setorder(forecast, forecast_date, location, target_end_date, type)
  
  # return output
  out <- list()
  out$weights <- weights
  out$forecast <- forecast
  return(out)
}

#' Train a Grid of quantgen Ensembles on a Target Window
#'
#' @param train_forecasts 
#' @param target_date 
#' @param train_window 
#' @param forecasts 
#' @import data.table
#' @return
#' @export
ensemble_grid <- function(train_forecasts, obs, target_date, train_window, train_horizons, forecasts){
  
  # extract training data
  message(sprintf("Generating training data with a window of %s and the following horizons: %s",
                  train_window, paste(train_horizons, collapse = ", ")))
  train_data <- extract_training_data(train_forecasts, obs, target_date, train_window, train_horizons)
  
  # define ensemble with inputs
  def_ensemble <- function(name, fit_args = NULL) {
    ensemble(name = name, train = train_data$train_array, true = train_data$true, 
             quantiles = train_data$tau, forecasts = forecasts, 
             models = train_data$models, fit_args = fit_args)
  }
  
  # overall
  message("Fitting overall ensemble")
  overall <- def_ensemble(name = "QRA")
   
  # CI weighted
  message("Fitting weighted CI ensemble")
  tau <- train_data$tau
  weighted_tau <- fcase(tau >= 0.4 & tau <= 0.6, 1,
                        tau >= 0.2 & tau < 0.4, 2,
                        tau < 0.2, 3,
                        tau > 0.6 & tau <= 0.8, 4,
                        tau > 0.8, 5)
  weighted_ci <- def_ensemble(name = "QRA (weighted quantiles)", fit_args = list(tau_groups = weighted_tau))
   
  # inverse weighted counts (assigning zero weight to 0 counts to avoid infinity)
  message("Fitting inverse counts ensemble")
  weights <- 1 / train_data$true
  weights <- ifelse(is.infinite(weights), 0, weights)
  weights <- weights / max(weights)
  
  inverse_counts <- def_ensemble(name = "QRA (inverse weighted counts)", fit_args = list(weights = weights))
  message("Fitting inverse counts weighted CI ensemble")
  # inverse weighted and weighted quantiles
  inverse_weighted <- def_ensemble(name = "QRA (weighted counts and quantiles)", 
                                   fit_args = list(tau_groups = weighted_tau, weights = weights))
  
  #combine QRA weights and forecasts
  message("Returning combined output")
  weights <- rbindlist(list(overall$weights,
                            weighted_ci$weights,
                            inverse_weighted$weights), use.names = TRUE)
  weights <- weights[, `:=`(window = train_window, horizons = paste(train_horizons, collapse = ", "))]
  
  forecasts <- rbindlist(list(overall$forecast,
                              weighted_ci$forecast,
                              inverse_weighted$forecast))
  forecasts <- forecasts[, `:=`(window = train_window, horizons = paste(train_horizons, collapse = ", "))]
  
  # return output
  out <- list()
  out$weights <- weights
  out$forecasts <- forecasts
  return(out)
}


#' Wrapper Compatible with future_lapply
#'
#' @param i 
#' @param train_forecasts 
#' @param obs 
#' @param target_date 
#' @param forecasts 
#' @return
#' @export
run_ensemble_grid <- function(i,train_forecasts, obs, 
                              target_date, forecasts) {
  out <- ensemble_grid(train_window = i$windows[[1]], train_horizons = i$horizons[[1]], 
                       train_forecasts = train_forecasts, obs = obs, 
                       target_date = target_date, forecasts = forecasts)
  return(out)
}