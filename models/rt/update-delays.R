# Packages ----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(future)
require(here)

# Save incubation period and generation time ------------------------------
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
saveRDS(generation_time , here("rt", "forecast", "delays", "data", "generation_time.rds"))
saveRDS(incubation_period, here("rt", "forecast", "delays", "data", "incubation_period.rds"))

# Set up parallel ---------------------------------------------------------
plan(multiprocess)

# Fit delay from onset to deaths ------------------------------------------
deaths <- readRDS(here("rt", "forecast", "delays", "data", "deaths.rds"))

onset_to_death_delay <- estimate_delay(deaths, bootstraps = 100, bootstrap_samples = 250, max_value = 30)

saveRDS(onset_to_death_delay, here("rt", "data", "delays", "onset_to_death_delay.rds"))

