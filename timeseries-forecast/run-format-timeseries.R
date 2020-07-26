# Format timeseries from samples to output ready for submission (or ensembling)
source(here::here("timeseries-forecast", "format-timeseries-fn.R"))
source(here::here("utils", "get-us-data.R"))
source(here::here("utils", "dates-to-epiweek.R"))
source(here::here("utils", "current-forecast-submission-date.R"))

# For formatting past timeseries samples:
# 
for(i in 
    c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", 
      "2020-07-13", "2020-07-20")){
  ##
  forecast_date <- i
  submission_date <- i
  ##
  
# Deaths only ------------------

model_type <- "deaths-only"

deaths_only <- format_timeseries(right_truncate_weeks = 1, 
                                 model_type = model_type,
                                 forecast_date = i,
                                 submission_date = i,
                                 quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
forecast_date <- unique(deaths_only$forecast_date)


# Save under "latest"
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, 
                                         "submission-files",
                                         paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_only, here::here("timeseries-forecast", model_type, "submission-files", "dated",
                                         paste0(i, "-", model_type, ".csv")))
                

# Deaths on cases -----------------

model_type <- "deaths-on-cases"
deaths_on_cases <- format_timeseries(right_truncate_weeks = 1, 
                                     model_type = model_type,
                                     forecast_date = i,
                                     submission_date = i,
                                     quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
forecast_date <- unique(deaths_on_cases$forecast_date)

# Save under "latest"
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, 
                                             "submission-files",
                                             paste0("latest-weekly-", model_type, ".csv")))
# Save under forecast date
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", model_type, "submission-files", "dated",
                                             paste0(i, "-", model_type, ".csv")))


##
}
##

