# Packages
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))

# Add state - model combinations here
swap <- list(
  # "mean" = c(
  #
  # ),
  # 
  # "Timeseries" = c(
  # 
  # ),
  # 
  "Rt" = c(
    "Wisconsin"
  )
  #,
  # "Case convolution" = c(
  # 
  # )
)

saveRDS(swap, here("submissions", "submission-notes", paste0(target_date, "-swap-ensemble.rds")))

# Update submission
source(here("submissions", "finalise.R"))
