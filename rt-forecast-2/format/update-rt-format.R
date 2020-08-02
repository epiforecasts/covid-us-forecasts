# Format output from Epinow2
# Packages ----------------------------------------------------------------
require(EpiNow2)
require(here)
require(stringr)
require(dplyr)
require(purrr)
require(ggplot2)
require(cowplot)

# Control parameters ------------------------------------------------------

forecast_dir <- here::here("rt-forecast-2/forecast/deaths") 

# Load forecasts ----------------------------------------------------------

# Latest forecast
source(here::here("utils", "current-forecast-submission-date.R"))

forecasts_raw <- EpiNow2::get_regional_results(results_dir = here::here("rt-forecast-2/forecast/deaths/state"),
                                              date = "latest", forecast = TRUE)$estimated_reported_cases$samples


# Format past forecasts ---------------------------------------------------
# 
# forecast_date <- "2020-07-19"
# submission_date <- "2020-07-20"
# 
# forecasts_raw <- EpiNow2::get_regional_results(results_dir = here::here("rt-forecast-2/forecast/deaths/state"),
#                                                date = forecast_date, forecast = TRUE)
# forecasts_raw <- forecasts_raw$estimated_reported_cases$samples

# Format samples ----------------------------------------------------------
setnames(forecasts_raw, old = c("region", "cases"), new = c("state", "deaths"))

forecasts_samples <- forecasts_raw[, .(sample = sample,
                                       deaths = deaths,
                                       target_end_date = date,
                                       forecast_date = forecast_date,
                                       model = "Rt-Epinow2",
                                       location = state)]

# Save samples
saveRDS(forecasts_samples, 
        paste0("rt-forecast-2/output/samples/", forecast_date, "-rt-forecast-samples.rds"))


# Format forecasts --------------------------------------------------------
source(here::here("rt-forecast-2/format/utils/format_forecast_us.R"))

formatted_forecasts <- format_forecast_us(forecasts = forecasts_raw, 
                                          forecast_date = forecast_date, 
                                          submission_date = submission_date)

# formatted_forecasts <- formatted_forecasts[target_end_date > forecast_date]

# Save forecast -----------------------------------------------------------

readr::write_csv(formatted_forecasts, 
                 paste0("rt-forecast-2/output/submission-files/dated/", forecast_date, "-rt-2-forecast.csv"))

readr::write_csv(formatted_forecasts, 
                 paste0("rt-forecast-2/output/submission-files/latest-rt-2-forecast.csv"))


