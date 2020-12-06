# Packages -----------------------------------------------------------------
library(EpiNow2, quietly = TRUE)
library(data.table, quietly = TRUE)
library(future, quietly = TRUE)
library(here, quietly = TRUE)
library(lubridate, quietly = TRUE)

# Set target date ---------------------------------------------------------
target_date <- as.character(Sys.Date()) 

# Update delays -----------------------------------------------------------
generation_time <- readRDS(here("rt-forecast", "data", "delays", "generation_time.rds"))
incubation_period <- readRDS(here("rt-forecast", "data" ,"delays", "incubation_period.rds"))
onset_to_report <- readRDS(here("rt-forecast", "data", "delays", "onset_to_report.rds"))

# Get cases  ---------------------------------------------------------------
cases <- fread(file.path("data", "daily-incidence-cases-Germany_Poland.csv"))
cases <- cases[, .(region = as.character(location_name), date = as.Date(date), 
                   confirm = value)]
cases <- cases[date >= (max(date) - weeks(12))]
data.table::setorder(cases, region, date)

# Set up parallel execution -----------------------------------------------
no_cores <- setup_future(cases)

# Run Rt estimation -------------------------------------------------------
rt <- opts_list(rt_opts(prior = list(mean = 1.1, sd = 0.2), 
                        future = "latest"), cases)
# add population adjustment for each country
rt$Germany$pop <- 80000000
rt$Poland$pop <- 40000000

regional_epinow(reported_cases = cases,
                generation_time = generation_time, 
                delays = delay_opts(incubation_period, onset_to_report),
                rt = rt,
                stan = stan_opts(samples = 2000, warmup = 250, 
                                 chains = 4, cores = no_cores),
                obs = obs_opts(scale = list(mean = 0.5, sd = 0.05)),
                horizon = 30,
                output = c("region", "summary", "timing", "samples"),
                target_date = target_date,
                target_folder = here("rt-forecast", "data", "samples", "cases"), 
                summary_args = list(summary_dir = here("rt-forecast", "data", "summary", 
                                                       "cases", target_date),
                                    all_regions = TRUE),
                logs = "rt-forecast/logs/cases", verbose = TRUE)

plan("sequential")