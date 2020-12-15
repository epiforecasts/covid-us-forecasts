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

# Make ensembles ----------------------------------------------------------
# simple ensemble over all time
overall <- quantile_ensemble(train_array, true_values, tau)

# construction CI weighting
weighted_tau <- fcase(tau >= 0.4 & tau <= 0.6, 1,
                      tau >= 0.2 & tau < 0.4, 2,
                      tau < 0.2, 3,
                      tau > 0.6 & tau <= 0.8, 4,
                      tau > 0.8, 5)
weighted_ci <- quantile_ensemble(train_array, true_values, tau,
                                 tau_groups = weighted_tau)

# inverse weight counts (assigning zero weight to 0 counts to avoid infinity)
weights <- 1 / true_values
weights <- ifelse(is.infinite(weights), 0, weights)
weights <- weights / max(weights)

inverse_counts <- quantile_ensemble(train_array, true_values, tau,
                                       weights = weights,
                                       tau_groups = weighted_tau)
# Save ensembles ----------------------------------------------------------
ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "weighted", paste0(target_date, ".csv")))




