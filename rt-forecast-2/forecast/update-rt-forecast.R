# Packages -----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(dplyr)

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
# all_dates <- readRDS(here::here("utils", "all_dates.rds"))
# submission_dates <- sort(as.vector(all_dates$submissions), decreasing = TRUE)
# last_four_submissions <- submission_dates[2:5]
# 
# for(i in last_four_submissions){
#   
# submission_date <- i
# 
# deaths <- dplyr::filter(deaths, date < submission_date)

# Format for epinow2 ------------------------------------------------------

deaths <- setDT(deaths)
deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(12))], by = region]

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


# Set up directories for models -------------------------------------------
models <- list("original", "fixed_rt")

targets <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/state"))
names(targets) <- models

summary <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/summary"))
names(summary) <- models

  
# Run Rt - ORIGINAL -------------------------------------------------------

  regional_epinow(reported_cases = deaths,
                generation_time = generation_time,
                delays = list(incubation_period, reporting_delay),
                horizon = 30,
                samples = 2000,
                warmup = 500,
                burn_in = 14,
                adapt_delta = 0.98,
                cores = no_cores,
                chains = ifelse(no_cores <= 2, 2, no_cores),
                target_folder = targets[["original"]],
                summary_dir = summary[["original"]],
                return_estimates = FALSE, verbose = FALSE)
  
# Run Rt - FIXED RT --------------------------------------------------

  regional_epinow(reported_cases = deaths,
                  generation_time = generation_time,
                  delays = list(incubation_period, reporting_delay),
                  horizon = 30,
                  samples = 2000,
                  warmup = 500,
                  burn_in = 14,
                  adapt_delta = 0.98,
                  fixed_future_rt = TRUE,
                  cores = no_cores,
                  chains = ifelse(no_cores <= 2, 2, no_cores),
                  target_folder = targets[["fixed_rt"]],
                  summary_dir = summary[["fixed_rt"]],
                  return_estimates = FALSE, verbose = FALSE)

# Add more models here ----------------------------------------------------


# Past forecasts ----------------------------------------------------------
# End for loop
# }


