# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

swap <- list(
  "Timeseries" = c("Oklahoma"),
  "Rt" = c("Idaho", "Michigan", "Virginia"),
  "Case convolution" = c("Minnesota"))

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# source(here("submissions", "finalise.R"))