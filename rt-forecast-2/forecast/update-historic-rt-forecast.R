
# Set submission dates ----------------------------------------------------

# all_dates <- readRDS(here::here("utils", "all_dates.rds"))
# submission_dates <- sort(as.vector(all_dates$rt_forecasts), decreasing = TRUE)
# submission_dates <- submission_dates[1:5]

# Get last 4 dates from a previously run Rt model
submission_dates <- dir("rt-forecast-2/output/original/submission-files/dated")
submission_dates <- gsub(pattern = "-rt-2-forecast\\.csv", replacement = "", x = submission_dates)
submission_dates <- as.Date(sort(submission_dates, decreasing = TRUE))[1:4]

# Load in Rt forecast data ------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "get-default-rt-data.R"))

# Get rt forecasts --------------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "run-rt-forecast.R"))

# Select models to run ----------------------------------------------------

models <- list("backcalc")

# Run forecast for current data -------------------------------------------

purrr::walk(submission_dates, ~ run_rt_forecast(deaths = deaths, 
                                                models = models,
                                                submission_date = .))

