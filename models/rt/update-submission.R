# Package -----------------------------------------------------------------
library(data.table)
library(EpiNow2)
library(lubridate)
library(here)
library(data.table)

# Dates -------------------------------------------------------------------
target_date <- readRDS(here("data", "target_date.rds"))

# Get forecasts -----------------------------------------------------------
forecast <- suppressWarnings(
  get_regional_results(results_dir = here("models", "rt", "data", "samples"),
                       date = ymd(target_date), forecast = TRUE, 
                       samples = TRUE)$estimated_reported_cases$samples)

# Format forecasts --------------------------------------------------------
source(here("utils", "format-forecast-us.R"))
setnames(forecast, c("cases", "region"), c("deaths", "state"))
formatted_forecasts <- format_forecast_us(forecasts = forecast,
                                          forecast_date = target_date, 
                                          submission_date = target_date,
                                          shrink_per = 0.1)

# Save submissions --------------------------------------------------------
source(here("utils", "check_dir.R"))
# write to disk
dated_submission <- here("models", "rt", "data", "submission", "dated")
check_dir(dated_submission)
fwrite(formatted_forecasts, here("models", "rt", "data", "submission", "latest.csv"))
fwrite(formatted_forecasts, paste0(dated_submission, "/", target_date, ".csv"))
