# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(quantgen)
library(purrr)
library(stringr)
library(lubridate)
library(future)
library(future.apply)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Training ----------------------------------------------------------------
train_windows <- c(2, 4, 8, 12)
train_horizons <- list(1:4, 1, 2, 4)

# Observations ------------------------------------------------------------
# requires Rt model to have been fit for this period
source(here("utils", "load_observations.R")) 
obs <- load_observations(target_date)
obs <- obs[,.(location, target_end_date = date, value)]
obs <- obs[!(location %in% "US")]

# Load models -------------------------------------------------------------
# load all single model forecasts
forecasts <- list.files(here("submissions", "all-models"))
forecasts <- map(forecasts, ~ fread(here("submissions", "all-models", .)))
forecasts <- rbindlist(forecasts)[grepl("inc", target)]

# filter out baseline models for ensembling
forecasts <- forecasts[!(model == "Baseline")]

# get current forecast
current_forecasts <- forecasts[forecast_date == target_date]
# exclude US from training
train_forecasts <- copy(forecasts)[!(location %in% "US")]

# Run Ensemble grids ------------------------------------------------------
# get ensemble tools
source(here("ensembles", "utils", "weighted-tools.R"))

# define grid
ensembles <- expand.grid(windows = train_windows, horizons = train_horizons)
ensembles <- as.data.table(ensembles)[, id := 1:.N]

# prune grid of impossible combinations
ensembles <- ensembles[, min_horizon := map(horizons, min)][windows >= min_horizon]

# set run grid in parallel
plan("multisession", earlySignal = TRUE)
ensembles <- future_lapply(split(ensembles, by = "id"), run_ensemble_grid,
                           train_forecasts = train_forecasts, obs = obs, 
                           target_date = target_date, forecasts = current_forecasts,
                           verbose = FALSE,
                           future.globals = c("ensemble_grid", "extract_training_data",
                                              "ensemble"),
                           future.packages = c("data.table", "lubridate", "quantgen",
                                               "purrr", "stringr"),
                           future.seed = TRUE)
plan("sequential")

# organise output
ensembles <- transpose(ensembles)
ensembles$weights <- rbindlist(ensembles$weights)
ensembles$forecasts <- rbindlist(ensembles$forecasts)

# Save ensembles ----------------------------------------------------------
fwrite(ensembles$forecasts, here("ensembles", "data", "weighted", "forecasts", paste0(target_date, ".csv")))
fwrite(ensembles$weights, here("ensembles", "data", "weighted", "weights", paste0(target_date, ".csv")))



