# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(quantgen)
library(purrr)
library(stringr)
library(lubridate)

# Target date -------------------------------------------------------------
#target_date <- as.Date(readRDS(here("data", "target_date.rds")))
target_date <- as.Date("2020-08-10")

# Training ----------------------------------------------------------------
train_windows <- c(2, 4, 8)

source(here("utils", "load_observations.R"))
obs <- load_observations(target_date)
obs <- obs[,.(location, target_end_date = date, value)]
obs <- obs[!(location %in% "US")]

# Load models -------------------------------------------------------------
forecasts <- list.files(here("submissions", "all-models"))
forecasts <- map(forecasts, ~ fread(here("submissions", "all-models", .)))
forecasts <- rbindlist(forecasts)[grepl("inc", target)]

# get current forecast
current_forecasts <- forecasts[forecast_date == target_date]
# exclude US from training
train_forecasts <- copy(forecasts)[!(location %in% "US")]

# Run Ensemble grids ------------------------------------------------------
source(here("ensembles", "models", "utils", "weighted-tools.R"))
ensembles <- ensemble_grid(train_forecasts, obs, target_date, train_window = 6, 
                           train_horizons = 2, current_forecasts)


# get forecasts for training
train_forecasts <- forecasts[forecast_date < target_date & forecast_date >= (target_date - weeks(train_window))]
train_forecasts <- train_forecasts[!(location %in% "US")]

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


#combine QRA weights and forecasts
weights <- rbindlist(list(overall$weights,
                          weighted_ci$weights,
                          inverse_weighted$weights), use.names = TRUE)

forecasts <- rbindlist(list(overall$forecast,
                            weighted_ci$forecast,
                            inverse_weighted$forecast))

# Save ensembles ----------------------------------------------------------



ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "weighted", paste0(target_date, ".csv")))




