# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(dplyr)

# Update delays -----------------------------------------------------------

generation_time <- readRDS(here::here("rt-forecast-2", "forecast", "delays", "data", "generation_time.rds"))
incubation_period <- readRDS(here::here("rt-forecast-2","forecast", "delays", "data", "incubation_period.rds"))
reporting_delay <- readRDS(here::here("rt-forecast-2","forecast", "delays", "data", "onset_to_death_delay.rds"))


## Define a very short and very tight reporting + incubation period. In a future version of epinow this will be replace with the
## ability to fit directly to the data
minimal_delay <- list(mean = log(1),
                      mean_sd = 0.01,
                      sd = log(1),
                      sd_sd = 0.01,
                      max = 10)
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