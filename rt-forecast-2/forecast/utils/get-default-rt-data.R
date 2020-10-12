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


# # # Set up cores -----------------------------------------------------
no_cores <- setup_future(deaths)
