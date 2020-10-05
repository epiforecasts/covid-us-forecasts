# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(dplyr)

# Set up logging ----------------------------------------------------------
setup_logging("INFO")
setup_logging("INFO", file = "info.log",
              name = "EpiNow.epinow")

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


# # # Set up cores -----------------------------------------------------
setup_future <- function(jobs, min_cores_per_worker = 4) {
  if (!interactive()) {
    ## If running as a script enable this
    options(future.fork.enable = TRUE)
  }
  
  workers <- min(ceiling(future::availableCores() / min_cores_per_worker), jobs)
  cores_per_worker <- max(1, round(future::availableCores() / workers, 0))
  
  futile.logger::flog.info("Using %s workers with %s cores per worker",
                           workers, cores_per_worker)
  
  
  future::plan(list(future::tweak(future::multiprocess, workers = workers, gc = TRUE, earlySignal = TRUE), 
                    future::tweak(future::multiprocess, workers = cores_per_worker)))
  return(cores_per_worker)
}

no_cores <- setup_future(length(unique(deaths$region)))
