# Package -----------------------------------------------------------------
library(data.table)
library(EpiNow2)
library(lubridate)
library(here)
library(data.table)

# Dates -------------------------------------------------------------------
target_date <- Sys.Date() - 7

# Get forecasts -----------------------------------------------------------
case_forecast <- suppressWarnings(
  EpiNow2::get_regional_results(results_dir = here("rt", "data", "samples"),
                                date = ymd(target_date), forecast = TRUE, 
                                samples = TRUE)$estimated_reported_cases$samples)

# Format forecasts --------------------------------------------------------
source(here("utils", "format-forecast-us.R"))
setnames(formatted_forecasts, c("value", "region"), c("deaths", "state"))
formatted_forecasts <- format_forecast_us(forecasts = case_forecast,
                                          forecast_date = target_date - 1, 
                                          submission_date = target_date - 1,
                                          shrink_per = 0)



# Save submissions --------------------------------------------------------
summarised_path <- here("rt", "data", "submission", target_date)

source(here("utils", "check_dir.R"))
check_dir(samples_path)
check_dir(summarised_path)

# save summary and samples
fwrite(forecast$samples, file.path(samples_path, "samples.csv"))
fwrite(forecast$summarised, file.path(summarised_path, "summary.csv"))

# save formatted forecasts - repurpose the Rt submission function

# write to disk
dated_submission <- here("rt", "data", "submission", "dated")
check_dir(dated_submission)
fwrite(formatted_forecasts, here("rt", "data", "submission", "latest.csv"))
fwrite(formatted_forecasts, paste0(dated_submission, "/", target_date, ".csv"))
