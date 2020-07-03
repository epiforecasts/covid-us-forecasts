# Format timeseries in output ready for submission (or ensembling)
source(here::here("timeseries-forecast", "format-timeseries-fn.R"))

# Run function for each model type ----------------------------------------

# Deaths only

model_type <- "deaths-only"
deaths_only <- format_timeseries(right_truncate_weeks = 1, model_type = model_type)
forecast_date <- unique(deaths_only$forecast_date)
# Save under "latest"
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, 
                                         "submission-files",
                                         paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, "submission-files", 
                                         paste0(forecast_date, "-", model_type, ".csv")))
                

# Deaths on cases

model_type <- "deaths-on-cases"
deaths_on_cases <- format_timeseries(right_truncate_weeks = 1, model_type = model_type)
forecast_date <- unique(deaths_on_cases$forecast_date)

# Save under "latest"
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, 
                                             "submission-files",
                                             paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, "submission-files", 
                                             paste0(forecast_date, "-", model_type, ".csv")))





