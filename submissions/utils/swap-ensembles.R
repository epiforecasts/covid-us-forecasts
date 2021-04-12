# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Add states here
swap <- list(
  # "mean" = c(),
  "median" = c("Massachusetts", "Nevada", "New Jersey", "Puerto Rico"),
  # "Timeseries" = c(),
  # "Rt" = c(),
  "Case convolution" = c("California")
)

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# Reset
swap <- list(
  "mean" = c(),
  "median" = c(),
  "Timeseries" = c(),
  "Rt" = c())

source(here("submissions", "finalise.R"))