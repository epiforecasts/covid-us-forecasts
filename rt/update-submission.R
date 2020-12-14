# Package -----------------------------------------------------------------
library(data.table)
library(EpiNow2)
library(lubridate)
library(here)
library(data.table)

# Dates -------------------------------------------------------------------
dates <- as.character(Sys.Date() - 7 * 4:0)
for (target_date in dates) {

  target_date <- ymd(target_date)
# Get forecasts -----------------------------------------------------------
forecast <- suppressWarnings(
  get_regional_results(results_dir = here("rt", "data", "samples"),
                       date = ymd(target_date), forecast = TRUE, 
                       samples = TRUE)$estimated_reported_cases$samples)

# Format forecasts --------------------------------------------------------
source(here("utils", "format-forecast-us.R"))
setnames(forecast, c("cases", "region"), c("deaths", "state"))
formatted_forecasts <- format_forecast_us(forecasts = forecast,
                                          forecast_date = target_date - 1, 
                                          submission_date = target_date - 1,
                                          shrink_per = 0.2)

# Save submissions --------------------------------------------------------
summarised_path <- here("rt", "data", "submission", target_date)
source(here("utils", "check-dir.R"))
check_dir(summarised_path)

# write to disk
dated_submission <- here("rt", "data", "submission", "dated")
check_dir(dated_submission)
fwrite(formatted_forecasts, here("rt", "data", "submission", "latest.csv"))
fwrite(formatted_forecasts, paste0(dated_submission, "/", target_date, ".csv"))
}