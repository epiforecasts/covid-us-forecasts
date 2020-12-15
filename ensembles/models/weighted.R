# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(quantgen)
library(purrr)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Load models -------------------------------------------------------------
forecasts <- list.files(here("submissions", "all-models"))
forecasts <- map()
source(here("utils", "load_submissions.R"))
forecasts <- load_submissions(target_date, "all-models", summarise = FALSE)


?combine_into_array()
model_weights <- quantgen::quantile_ensemble

# Save ensembles ----------------------------------------------------------
ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "weighted", paste0(target_date, ".csv")))




