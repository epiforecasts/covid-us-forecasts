# Packages
library(here)

target_date <- as.Date(readRDS(here("data", "target_date.rds")))
state_codes <- readRDS(here("data", "state_codes.rds"))

# Add state - model combinations here
swap <- list(
  # "mean" = c(
  #
  # ),
  # "median" = c( 
  # )
  # ,
  # # 
  # "Timeseries" = c(
  #  "Tennessee"
  # )
  # ,
  # 
  "Rt" = c(
    "Florida"
  )
  #,
  # "Case convolution" = c(
  #   ""
  # )
)

saveRDS(swap, here("submissions", "submission-notes", paste0(target_date, "-swap-ensemble.rds")))

# Update submission
source(here("submissions", "finalise.R"))
