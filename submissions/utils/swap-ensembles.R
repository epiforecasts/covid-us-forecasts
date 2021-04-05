# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Add states here
swap <- list(
  # "mean" = c(),
  # "median" = c(),
  "Timeseries" = c("Michigan", "New Jersey", "North Dakota", "Pennsylvania", "US"),
  "Rt" = c("Illinois", "Massachusetts", "Indiana", "Maryland", "New Hampshire", "Minnesota"))

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# Reset
swap <- list(
  "mean" = c(),
  "median" = c(),
  "Timeseries" = c(),
  "Rt" = c())

# source(here("submissions", "finalise.R"))