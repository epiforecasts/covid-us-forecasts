

# install.packages("drat")
# drat::addRepo("epiforecasts")
# devtools::install_dev_deps()


# # Packages -----------------------------------------------------------------
require(EpiNow)
require(data.table)
require(forecastHybrid)
require(future)
require(dplyr)
require(tidyr)
require(magrittr)


# Update delays -----------------------------------------------------------

# source(here::here("rt-forecast", "utils", "update-delays.R"))

# Run Rt ------------------------------------------------------------------

source(here::here("rt-forecast", "utils", "run-rt-forecast.R"))

# run forecasts
run_rt_forecast()


# Format for submission ---------------------------------------------------

source(here::here("utils", "format-for-submission.R"))
