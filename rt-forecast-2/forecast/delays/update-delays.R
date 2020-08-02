# Packages ----------------------------------------------------------------

require(EpiNow2)
require(data.table)
require(future)



# Save incubation period and generation time ------------------------------

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)


saveRDS(generation_time , here::here("rt-forecast-2", "forecast", "delays", "data", "generation_time.rds"))
saveRDS(incubation_period, here::here("rt-forecast-2", "forecast", "delays", "data", "incubation_period.rds"))


# Set up parallel ---------------------------------------------------------

if (!interactive()) {
  ## If running as a script enable this
  options(future.fork.enable = TRUE)
}


plan(multiprocess)

# Fit delay from onset to admission ---------------------------------------

# linelist <- readRDS(here::here("delays", "data", "pseudo_linelist.rds"))

# linelist <- data.table::as.data.table(linelist)

# onset_to_admission_delay <- EpiNow2::bootstrapped_dist_fit(linelist$report_delay, bootstraps = 100, 
#                                                           bootstrap_samples = 250)
## Set max allowed delay to 30 days to truncate computation
# onset_to_admission_delay$max <- 30

# saveRDS(onset_to_admission_delay, here::here("delays", "data", "onset_to_admission_delay.rds"))

# Fit delay from onset to deaths ------------------------------------------

deaths <- readRDS(here::here("rt-forecast-2", "forecast", "delays", "data", "deaths.rds"))

onset_to_death_delay <- EpiNow2::bootstrapped_dist_fit(deaths, bootstraps = 100, bootstrap_samples = 250)
## Set max allowed delay to 30 days to truncate computation
onset_to_death_delay$max <- 30

saveRDS(onset_to_death_delay, here::here("rt-forecast-2", "forecast", "delays", "data", "onset_to_death_delay.rds"))

