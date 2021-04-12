# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Add states here
swap <- list(
  # "mean" = c(),
  # "median" = c(),
  # "Timeseries" = c(),
  "Rt" = c("Arizona", "California", "Illinois", "Florida", 
           "Massachusetts", "Nevada", "New Jersey", "Washington")
)

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# Reset
swap <- list(
  "mean" = c(),
  "median" = c(),
  "Timeseries" = c(),
  "Rt" = c())

source(here("submissions", "finalise.R"))