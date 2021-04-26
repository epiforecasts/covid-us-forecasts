# Packages
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Add states here
swap <- list(
  # "mean" = c(
  #
  # ),
  # 
  # "Timeseries" = c(
  # 
  # ),
  # 
  "Rt" = c("Maryland",
           "Michigan",
           "Oklahoma",
           "Ohio",
           "US"
  )
  #,
  # "Case convolution" = c(
  # 
  # )
)

saveRDS(swap, here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))

# Update submission
source(here("submissions", "finalise.R"))

# Reset
swap <- list(
  "mean" = c(),
  "median" = c(),
  "Timeseries" = c(),
  "Rt" = c())