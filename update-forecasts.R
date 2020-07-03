# Source this script to run a complete submission update end-to-end

# Update data -------------------------------------------------------------

source(here::here("utils", "get-us-data.R"))

# Update Rt forecast ------------------------------------------------------

source(here::here("rt-forecast", "update-delays.R"))
source(here::here("rt-forecast", "update-us-deaths-rt.R"))
source(here::here("rt-forecast", "run-format-rt.R"))

# Update time series models -----------------------------------------------

source(here::here("timeseries-forecast", "update-timeseries.R"))
source(here::here("timeseries-forecast", "run-format-timeseries.R"))

# Ensemble ----------------------------------------------------------------
# - Using mean ensemble
source(here::here("ensembling", "old-quantile-average.R"))

# Visualisation -----------------------------------------------------------

source(here::here("evaluation", "post-ensemble-plot.R"))

# Format states with >100 cases in submission csv -------------------------

source(here::here("final-submissions", "update-final-submission.R"))

# Submit ------------------------------------------------------------------
# See: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md

