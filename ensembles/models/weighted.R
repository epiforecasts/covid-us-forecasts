# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(quantgen)
library(purrr)
library(stringr)
library(lubridate)

# Target date -------------------------------------------------------------
#target_date <- as.Date(readRDS(here("data", "target_date.rds")))
target_date <- as.Date("2020-12-14")

# Training ----------------------------------------------------------------
train_window <- 4

source(here("utils", "load_observations.R"))
obs <- load_observations(target_date)
obs <- obs[,.(location, target_end_date = date, value)]
obs <- obs[!(location %in% "US")]

# Load models -------------------------------------------------------------
forecasts <- list.files(here("submissions", "all-models"))
forecasts <- map(forecasts, ~ fread(here("submissions", "all-models", .)))
forecasts <- rbindlist(forecasts)[grepl("inc", target)]
forecasts <- forecasts[!(model %in% "Timeseries")]

# get current forecast
current_forecast <- forecasts[forecast_date == target_date]

# get forecasts for training
train_forecasts <- forecasts[forecast_date < target_date & forecast_date >= (target_date - weeks(train_window))]
train_forecasts <- train_forecasts[!(location %in% "US")]

# throw error if not enough training forecasts for specified window
if (length(unique(train_forecasts$forecast_date)) != train_window) {
 stop("Insufficient training data for specified window") 
}

# Munge training ----------------------------------------------------------
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


# Define generic QRA ------------------------------------------------------
ensemble <- function(name, train = train_array, true = true_values, quantiles = tau, 
                     models = unique(train_forecasts$model), fit_args = NULL,
                     forecast) {
  # fit QRA
  fit <- do.call(quantile_ensemble, 
                 c(list(qarr = train, y = true, tau = quantiles), fit_args))
  # extract weights
  weights <- data.table(ensemble = name, model = models, weight = round(fit$alpha, 3))
  
  # allocate weights depending on if by quantile or not
  if (length(fit$alpha) == length(models)) {
    weights <- weights[, quantile := list(tau)]
    weights <- weights[, .(quantile = unlist(quantile)), by = setdiff(colnames(weights), "quantile")]
  }else{
    weights <- melt(weights, id.vars = c("ensemble", "model"), variable.name = "quantile",
                    value.name = "weight")
    weights <- weights[, quantile := str_remove_all(quantile, "weight.")]
  }
  
  #add NA (i.e point) and order
  weights <- rbindlist(list(
    weights, 
    copy(weights)[quantile == 0.5][, quantile := NA]
  ))
  setorder(weights, ensemble, quantile, model)
  
  out <- list()
  out$weights <- weights
  return(out)
}

# Generate QRAs -----------------------------------------------------------
# overall
overall <- ensemble(name = "QRA")

# CI weighted
weighted_tau <- fcase(tau >= 0.4 & tau <= 0.6, 1,
                      tau >= 0.2 & tau < 0.4, 2,
                      tau < 0.2, 3,
                      tau > 0.6 & tau <= 0.8, 4,
                      tau > 0.8, 5)
weighted_ci <- ensemble(name = "QRA (weighted quantiles)", fit_args = list(tau_groups = weighted_tau))

# inverse weighted counts (assigning zero weight to 0 counts to avoid infinity)
weights <- 1 / true_values
weights <- ifelse(is.infinite(weights), 0, weights)
weights <- weights / max(weights)

inverse_counts <- ensemble(name = "QRA (inverse weighted counts)", fit_args = list(weights = weights))

# inverse weighted and weighted quantiles
inverse_weighted <- ensemble(name = "QRA (weighted counts and quantiles)", 
                             fit_args = list(tau_groups = weighted_tau, weights = weights))

# Save ensembles ----------------------------------------------------------
ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "weighted", paste0(target_date, ".csv")))




