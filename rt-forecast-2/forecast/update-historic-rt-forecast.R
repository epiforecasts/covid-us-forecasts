
# Set submission dates ----------------------------------------------------

all_dates <- readRDS(here::here("utils", "all_dates.rds"))
submission_dates <- sort(as.vector(all_dates$rt_forecasts), decreasing = TRUE)
submission_dates <- submission_dates[2:5]

# Load in Rt forecast data ------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "get-default-rt-data.R"))

# Get rt forecasts --------------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "run-rt-forecast.R"))

# Run forecast for current data -------------------------------------------

purrr::walk(submission_dates, ~ run_rt_forecast(deaths = deaths, submission_date = .))

