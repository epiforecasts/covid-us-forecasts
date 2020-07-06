# Forecast deaths from estimated infections (Rt) 
# 
# Packages -----------------------------------------------------------------
# 
# require(drat)
drat::addRepo("epiforecasts")
# install.packages("EpiSoon")
require(devtools)
devtools::install_deps()
devtools::install_deps(repos = "https://epiforecasts.io/drat/")

require(EpiNow)
require(data.table)
require(forecastHybrid)
require(future)
require(dplyr)
require(tidyr)
require(magrittr)

# Define a target date ----------------------------------------------------

target_date <- Sys.Date()

# Read in delay from onset to death --------------------------------------------------------

delay_dists <- readRDS(here::here("rt-forecast", "data", "onset_to_death_delay.rds"))

# Read in incubation period -----------------------------------------------

incubation_defs <- readRDS(here::here("rt-forecast", "data", "incubation.rds"))

# Get and reshape deaths data ---------------------------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("utils", "adjust-data-anomalies.R"))

# Get raw data
raw_deaths <- get_us_deaths(data = "daily")

# Adjust deaths for massive anomalies (1000% change)
deaths <- adjust_data_anomalies(data = raw_deaths, variable_name = "deaths", threshold = 100)

# Format for Epinow
deaths_national <- deaths %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(deaths = sum(deaths)) %>%
  dplyr::rename(local = deaths) %>% 
  dplyr::mutate(imported = 0, region = "US") %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 

deaths_regional <- deaths %>%
  dplyr::rename(local = deaths, region = state) %>% 
  dplyr::mutate(imported = 0) %>% 
  tidyr::gather(key = "import_status", value = "confirm", local, imported) 

max_date <- min(data.table::as.data.table(deaths_national)[, .SD[date == max(date)], by = region]$date)


# Define nowcast lag at the 40% quantile ----------------------------------

nowcast_lag <- 9 + 5 # Delay from death -> onset + onset -> infection

# # Set up cores -----------------------------------------------------
setup_future <- function(jobs) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }
  
  
  plan(list(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
            tweak(multiprocess, workers = max(1, round(future::availableCores() / jobs)))),
       gc = TRUE, earlySignal = TRUE)
}

# Estimate Rt and forecast death counts ----------------------------------------------------------------

## National

setup_future(length(unique(deaths_national$region)))

EpiNow::regional_rt_pipeline(
  # Settings to estimate Rt
  cases = deaths_national,
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "rt-forecast/national",
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 0,
  approx_delay = TRUE,
  # Settings for forecasting
  horizon = 40, report_forecast = TRUE,
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 40):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)})


## Regional 

setup_future(length(unique(deaths_regional$region)))

EpiNow::regional_rt_pipeline(
  # Settings to estimate Rt
  cases = deaths_regional, 
  delay_defs = delay_dists,
  incubation_defs = incubation_defs,
  target_folder = "rt-forecast/state", 
  target_date = target_date,
  nowcast_lag = nowcast_lag,
  case_limit = 0,
  min_forecast_cases = 0,
  approx_delay = TRUE,
  # Settings for forecasting
  horizon = 40, report_forecast = TRUE,
  forecast_model = function(y, ...){
    EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 40):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)})

# Summarise results -------------------------------------------------------


EpiNow::regional_summary(results_dir = "rt-forecast/national", 
                         summary_dir = "rt-forecast/national-summary",
                         target_date = "latest",
                         region_scale = "Country")


EpiNow::regional_summary(results_dir = "rt-forecast/state", 
                         summary_dir = "rt-forecast/state-summary",
                         target_date = "latest",
                         region_scale = "State")
