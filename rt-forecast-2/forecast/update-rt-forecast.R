# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(dplyr)

# Define a target date ----------------------------------------------------

# target_date <- Sys.Date()


# Update delays -----------------------------------------------------------

generation_time <- readRDS(here::here("rt-forecast-2", "forecast", "delays", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("rt-forecast-2","forecast", "delays", "data", "incubation_period.rds"))
reporting_delay <- readRDS(here::here("rt-forecast-2","forecast", "delays", "data", "onset_to_death_delay.rds"))


# Get deaths  ---------------------------------------------------------------
source(here::here("utils", "get-us-data.R"))

# Get raw data
deaths_raw <- get_us_deaths(data = "daily")

# Reshape
deaths_national <- deaths_raw %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  dplyr::rename(confirm = deaths) %>%
  dplyr::mutate(region = "US")

deaths <- deaths_raw %>%
  dplyr::rename(confirm = deaths, region = state) %>%
  dplyr::select(date, region, confirm) %>%
  dplyr::bind_rows(deaths_national)

# Format for epinow2 ------------------------------------------------------

deaths <- setDT(deaths)
deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(8))], by = region]

data.table::setorder(deaths, date)

# # Set up cores -----------------------------------------------------
setup_future <- function(jobs) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }


  plan(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
       gc = TRUE, earlySignal = TRUE)
  
  
  jobs <- max(1, ceiling(future::availableCores() / jobs))
  return(jobs)
}


no_cores <- setup_future(length(unique(deaths$region)))


# Run Rt estimation -------------------------------------------------------

regional_epinow(reported_cases = deaths,
                generation_time = generation_time,
                delays = list(incubation_period, reporting_delay),
                horizon = 42,
                samples = 2000,
                warmup = 500,
                adapt_delta = 0.99,
                cores = no_cores,
                chains = 4,
                target_folder = "rt-forecast-2/forecast/deaths/state",
                case_limit = 1,
                summary_dir = "rt-forecast-2/forecast/deaths/summary",
                return_estimates = FALSE, verbose = TRUE)
