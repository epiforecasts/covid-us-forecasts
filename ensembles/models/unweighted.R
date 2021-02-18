# Packages ----------------------------------------------------------------
library(data.table)
library(here)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Load models -------------------------------------------------------------
source(here("utils", "load_submissions.R"))
forecasts <- load_submissions(target_date, "all-models", summarise = FALSE)

# filter out baseline models for ensembling
forecasts <- forecasts[!(model == "Baseline")]


# Make ensembles ----------------------------------------------------------
cols <- setdiff(colnames(forecasts), c("value", "model"))
mean <- copy(forecasts)[, .(value = mean(value)), by = cols]
mean <- mean[, model := "mean"]

median <- copy(forecasts)[, .(value = median(value)), by = cols]
median <- median[, model := "median"]

# Save ensembles ----------------------------------------------------------
ensembles <- rbindlist(list(mean, median))
fwrite(ensembles, here("ensembles", "data", "unweighted", paste0(target_date, ".csv")))




