# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

swap <- list(
  "median" = c("US", "Colorado", "California", "Missouri", "Texas"))

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# source(here("submissions", "finalise.R"))