# Packages -----------------------------------------------------------------
library(EpiNow2, quietly = TRUE)
library(data.table, quietly = TRUE)
library(future, quietly = TRUE)
library(here, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(purrr, quietly = TRUE)

# Set target date ---------------------------------------------------------
target_date <- readRDS(here("data", "target_date.rds"))

# Update delays -----------------------------------------------------------
generation_time <- readRDS(here("models", "rt", "data", "delays", "generation_time.rds"))
incubation_period <- readRDS(here("models", "rt", "data" ,"delays", "incubation_period.rds"))
onset_to_report <- readRDS(here("models", "rt", "data", "delays", "onset_to_death_delay.rds"))

# Get cases  ---------------------------------------------------------------
source(here("utils", "get-us-data.R"))
deaths <- get_us_deaths(data = "daily")
deaths <- as.data.table(deaths)
deaths <- deaths[, .(region = state, date = as.Date(date), 
                   confirm = deaths)]
us_deaths <- copy(deaths)[, .(confirm = sum(confirm, na.rm = TRUE)), by = "date"]
us_deaths <- us_deaths[, region := "US"]
deaths <- rbindlist(list(us_deaths, deaths), use.names = TRUE)
deaths <- deaths[date <= as.Date(target_date)]
deaths <- deaths[date >= (as.Date(target_date) - weeks(12))]
setorder(deaths, region, date)

# Set up options ----------------------------------------------------------
# Add maximum susceptible population
rt <- opts_list(rt_opts(prior = list(mean = 1, sd = 0.2), 
                        future = "latest"), deaths)
pops <- fread(here("data", "state_pop_totals.csv"))
rt <- map(names(rt), function(x) {
  y <- rt[[x]]
  y$pop <- pops[state_name %in% x]$tot_pop
  return(y)
})
names(rt) <- unique(deaths$region)

# Set up parallel execution -----------------------------------------------
no_cores <- setup_future(deaths)

# Run Rt estimation -------------------------------------------------------
regional_epinow(reported_cases = deaths,
                generation_time = generation_time, 
                delays = delay_opts(incubation_period, onset_to_report),
                rt = rt,
                stan = stan_opts(samples = 2000, warmup = 250, 
                                 chains = 4, cores = no_cores),
                obs = obs_opts(scale = list(mean = 0.005, sd = 0.0025)),
                horizon = 30,
                output = c("region", "summary", "timing", "samples"),
                target_date = target_date,
                target_folder = here("models", "rt", "data", "samples"), 
                summary_args = list(summary_dir = here("models", "rt", "data", 
                                                       "summary", target_date),
                                    all_regions = FALSE),
                logs = "models/rt/logs/cases", verbose = FALSE)

plan("sequential")
