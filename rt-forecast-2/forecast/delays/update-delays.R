# Packages ----------------------------------------------------------------

require(EpiNow2)
require(data.table)
require(future)



# Save incubation period and generation time ------------------------------

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

saveRDS(generation_time , here::here("rt-forecast-2", "forecast", "delays", "data", "generation_time.rds"))
saveRDS(incubation_period, here::here("rt-forecast-2", "forecast", "delays", "data", "incubation_period.rds"))


# Set up parallel ---------------------------------------------------------

if (!interactive()) {
  ## If running as a script enable this
  options(future.fork.enable = TRUE)
}


plan(multiprocess)

# Fit delay from onset to deaths ------------------------------------------

deaths <- readRDS(here::here("rt-forecast-2", "forecast", "delays", "data", "deaths.rds"))

onset_to_death_delay <- EpiNow2::bootstrapped_dist_fit(deaths, bootstraps = 100,
                                                       bootstrap_samples = 250, max_value = 30)

saveRDS(onset_to_death_delay, here::here("rt-forecast-2", "forecast", "delays", "data", "onset_to_death_delay.rds"))

