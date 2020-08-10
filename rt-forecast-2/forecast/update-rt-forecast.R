# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(dplyr)

# Define a target date ----------------------------------------------------

# target_date <- Sys.Date()
start_time <- Sys.time()

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


# Past forecasts ----------------------------------------------------------
# submission_dates <- c("2020-07-27", "2020-07-20", "2020-07-13") # , "2020-07-06", "2020-06-29", "2020-06-22","2020-06-15")
# 
# for(i in submission_dates){
#   
# submission_date <- i
# 
# deaths_past <- dplyr::filter(deaths, date < submission_date)

# Format for epinow2 ------------------------------------------------------

deaths <- setDT(deaths)
deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(8))], by = region]

data.table::setorder(deaths, date)


# # # Set up cores -----------------------------------------------------
setup_future <- function(jobs) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }


  plan(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
       gc = TRUE, earlySignal = TRUE)


  jobs <- max(1, round(future::availableCores() / jobs, 0))
  return(jobs)
}


no_cores <- setup_future(length(unique(deaths$region)))

# Run Rt estimation -------------------------------------------------------
start_time <- Sys.time()

  regional_epinow(reported_cases = deaths,
                generation_time = generation_time,
                delays = list(incubation_period, reporting_delay),
                horizon = 30,
                samples = 2000,
                warmup = 200,
                adapt_delta = 0.95,
                cores = no_cores,
                chains = ifelse(no_cores <= 2, 2, no_cores),
                target_folder = "rt-forecast-2/forecast/deaths/state",
                summary_dir = "rt-forecast-2/forecast/deaths/summary",
                return_estimates = FALSE, verbose = FALSE)


# Past forecasts:
# End for loop
# }

end_time <- Sys.time()
run_time_mins <- end_time - start_time
time <- cbind(as.character(start_time), start_time, end_time, run_time_mins)

saveRDS(time, here::here("utils/epinow2_runtime.rds"))

# Settings - running past forecast, Alabama only:
# Base setting:
# horizon = 30, samples = 2000, warmup = 200, adapt_delta = 0.95, 
# cores = no_cores, chains = ifelse(no_cores<=2, 2, no_cores)
#
# Base setting: 4.5hr total
# run_time: 5.1 mins
# warnings:
# 35 divergent transitions
# running chains for more iterations may help

# adapt_delta = 0.99, warmup = 500
# run_time: 13min / 11.5hr
# warnings:

# adapt_delta = 0.99, warmup = 500, cores = 4, chains = 4
# run_time: 9.58min / 8.5 hr
# warnings:

# adapt_delta = 0.95, warmup = 200, cores = 4, chains = 4
# run_time: 4.4min / 3.8hr
# warnings: