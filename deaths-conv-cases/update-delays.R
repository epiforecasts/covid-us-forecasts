# Packages ----------------------------------------------------------------
library(EpiNow2, quietly = TRUE)
library(covidregionaldata, quietly = TRUE)
library(data.table, quietly = TRUE)
library(future, quietly = TRUE)
library(here, quietly = TRUE)

# Save incubation period and generation time ------------------------------
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani",
                                       max_value = 15)
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer",
                                           max_value = 15)
saveRDS(generation_time , here::here("deaths-conv-cases", "data", "delays", "generation_time.rds"))
saveRDS(incubation_period, here::here("deaths-conv-cases", "data", "delays", "incubation_period.rds"))

# Set up parallel ---------------------------------------------------------
future::plan("multiprocess")

# get linelist ------------------------------------------------------------
linelist <- data.table::as.data.table(covidregionaldata::get_linelist(clean = TRUE))
linelist <- linelist[country %in% "Germany"]

# Fit delay from onset to admission ---------------------------------------
report_delay <- data.table::copy(linelist)[!is.na(delay_onset_report)]
samples <- round(length(report_delay$delay_onset_report) / 100)
onset_to_report <- estimate_delay(report_delay$delay_onset_report,
                                  bootstraps = 100, bootstrap_samples = 1000,
                                  max_value = 15)
saveRDS(onset_to_report, here::here("deaths-conv-cases", "data", "delays", "onset_to_report.rds"))
