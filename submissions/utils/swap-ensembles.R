# Packages ----------------------------------------------------------------
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

swap <- list(
  "Case convolution" = c("Delaware", "Ohio", "Rhode Island", "Virginia", "Utah", "Hawaii", "Maine"),
  "Rt" = c("Arkansas", "Colorado", "Columbia"))

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-ensemble-swap.rds")))

# source(here("submissions", "finalise.R"))