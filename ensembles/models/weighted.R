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
obs <- obs[date > (target_date - weeks(train_window)), .(location, target_end_date = date, value)]
obs <- obs[!(location %in% "US")]

# Load models -------------------------------------------------------------
forecasts <- list.files(here("submissions", "all-models"))
forecasts <- map(forecasts, ~ fread(here("submissions", "all-models", .)))
forecasts <- rbindlist(forecasts)[grepl("inc", target)]

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
train_forecasts <- train_forecasts[obs, on = c("location", "target_end_date")]

# only train where all forecasts are present
train_forecasts <- train_forecasts[, `:=`(model_no = 1:.N, models = .N),
                                   by = .(forecast_date, target_end_date, location)]
train_forecasts <- train_forecasts[models == max(models)]

# make quantgen data format
true_values <- train_forecasts[model_no == 1]$value

train_array <- copy(train_forecasts)[, c("forecast_date", "target_")]

qarr <- combined %>%
  dplyr::select(-deaths) %>%
  dplyr::group_split(model, .keep = FALSE) %>%
  setNames(models) %>%
  purrr::map(.f = as.matrix) %>%
  quantgen::combine_into_array()


?combine_into_array()
model_weights <- quantgen::quantile_ensemble

# Save ensembles ----------------------------------------------------------
ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "weighted", paste0(target_date, ".csv")))




