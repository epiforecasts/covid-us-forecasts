# Packages -----------------------------------------------------------------
library(EpiNow2, quietly = TRUE)
library(data.table, quietly = TRUE)
library(future, quietly = TRUE)
library(here, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(purrr, quietly = TRUE)

dates <- c(ymd("2020-11-30"))
dates <- c(dates, map_chr(1:3, ~ as.character(dates - weeks(.))))
dates <- as.character(dates)
dates <- dates[-c(1,2)]

# Set target date ---------------------------------------------------------
#target_date <- as.character(Sys.Date()) 

for (target_date in dates) {
# Update delays -----------------------------------------------------------
generation_time <- readRDS(here("deaths-conv-cases", "data", "delays", "generation_time.rds"))
incubation_period <- readRDS(here("deaths-conv-cases", "data" ,"delays", "incubation_period.rds"))
onset_to_report <- readRDS(here("deaths-conv-cases", "data", "delays", "onset_to_report.rds"))

# Get cases  ---------------------------------------------------------------
source(here("utils", "get-us-data.R"))
cases <- get_us_cases(data = "daily")
cases <- as.data.table(cases)
cases <- cases[, .(region = state, date = as.Date(date), 
                   confirm = cases)]
us_cases <- copy(cases)[, .(confirm = sum(confirm, na.rm = TRUE)), by = "date"]
us_cases <- us_cases[, region := "US"]
cases <- rbindlist(list(us_cases, cases), use.names = TRUE)
cases <- cases[date >= (as.Date(target_date) - weeks(12))]
setorder(cases, region, date)

# Set up options ----------------------------------------------------------
# Add maximum susceptible population
rt <- opts_list(rt_opts(prior = list(mean = 1, sd = 0.2), 
                        future = "latest"), cases)
pops <- fread(here("utils", "state_pop_totals.csv"))
rt <- map(names(rt), function(x) {
  y <- rt[[x]]
  y$pop <- pops[state_name %in% x]$tot_pop
  return(y)
})
names(rt) <- unique(cases$region)

# Set up parallel execution -----------------------------------------------
no_cores <- setup_future(cases)

# Run Rt estimation -------------------------------------------------------
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
                target_folder = here("deaths-conv-cases", "data", "samples", "cases"), 
                summary_args = list(summary_dir = here("deaths-conv-cases", "data", 
                                                       "summary", "cases", target_date),
                                    all_regions = FALSE),
                logs = "deaths-conv-cases/logs/cases", verbose = FALSE)

plan("sequential")
}