
# ============================================================================ #
# Update data used for the forecasts
# ============================================================================ #

source(here::here("utils", "get-us-data.R"))


# update delays (not run)
# source(here::here("rt-forecast", "update-delays.R"))

# ============================================================================ #
# Run all models
# ============================================================================ #

# run rt forecast for current date
source(here::here("rt-forecast", "update-us-deaths-rt.R"))

# run timeseries forecasts
source(here::here("timeseries-forecast", "update-timeseries.R"))


# ============================================================================ #
# format submission
# ============================================================================ #

# source functions
source(here::here("utils", "format-submission-functions.R"))

# format timeseries forecasts
source(here::here("utils", "format-submission-timeseries.R"))

# format rt forecasts
source(here::here("utils", "format-submission-rt.R"))


# ============================================================================ #
# ensembling
# ============================================================================ #

source(here::here("ensemble-forecast", "qra-ensemble", "quantile-ensembling.R"))



