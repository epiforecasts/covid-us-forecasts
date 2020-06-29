# Packages ----------------------------------------------------------------

require(EpiNow)
require(data.table)
require(future)

# Set up parallel ---------------------------------------------------------
if (!interactive()) {
  ## If running as a script enable this
  options(future.fork.enable = TRUE)
}


plan(multiprocess)

# Fit delay from onset to admission ---------------------------------------
# 
# linelist <- readRDS(here::here("rt-forecast/data/pseudo_linelist.rds"))
# 
# linelist <- data.table::as.data.table(linelist)
# 
# onset_to_admission_delay <- 
#   EpiNow::get_dist_def(linelist$report_delay,
#                               samples = 1000, bootstraps = 100)  
# 
# 
# saveRDS(onset_to_admission_delay, here::here("rt-forecast", "data", "onset_to_admission_delay.rds"))
# 
# Fit delay from onset to deaths ------------------------------------------

deaths_delay <- readRDS(here::here("rt-forecast", "data", "deaths-delay.rds"))

onset_to_death_delay <- 
  EpiNow::get_dist_def(deaths_delay,
                       samples = 1000, bootstraps = 100)  

saveRDS(onset_to_death_delay, here::here("rt-forecast", "data", "onset_to_death_delay.rds"))

# Construct incubation period ---------------------------------------------

## Mean delay
exp(EpiNow::covid_incubation_period[1, ]$mean)

## Get incubation defs
incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 1000)

saveRDS(incubation_defs, here::here("rt-forecast", "data", "incubation.rds"))
