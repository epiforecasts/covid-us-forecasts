# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(ggplot2)
library(purrr)

# Target date -------------------------------------------------------------
target_date <- Sys.Date()


# Load observations -------------------------------------------------------
obs <- fread(here("models", "rt", "data", "summary", target_date, "reported_cases.csv"))
obs <- obs[, .(date, state = region, value = confirm)]

state_codes <- readRDS(here("data", "state_codes.rds"))
obs <- obs[state_codes, on = "state"]

source(here("utils", "dates-to-epiweek.R"))
obs <- dates_to_epiweek(obs)
obs <- obs[epiweek_full == TRUE, .(value = sum(value), date = max(date)), 
           by = .(location, state, epiweek)][, epiweek := NULL]
# Load models -------------------------------------------------------------

forecasts <- fread(here("submissions", "all-models", paste0(target_date, ".csv")))
forecasts <- forecasts[, .(date = target_end_date, location, quantile, value, model, target)]
forecasts[grepl("inc inc", target)]
forecasts <- forecasts[state_codes, on = "location"]

forecasts <- dcast(forecasts, ... ~ quantile, value.var = "value")
forecasts <- forecasts[!is.na(date)]
forecasts <- forecasts[, c("date", "location", "model", "state", "NA", "0.025", "0.25", "0.75", "0.975")]
setnames(forecasts, "NA", "value")
