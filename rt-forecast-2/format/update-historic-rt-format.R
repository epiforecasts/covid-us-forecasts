
# past forecasts ---------------------------------------------------
all_dates <- readRDS(here::here("utils", "all_dates.rds"))
forecast_dates <- sort(as.vector(all_dates$rt_forecasts), decreasing = TRUE)
forecast_dates <- forecast_dates[1:5]

submission_dates <- sort(as.vector(all_dates$submissions), decreasing = TRUE)
submission_dates <- submission_dates[1:5]

# Get format function -----------------------------------------------------
source(here::here("rt-forecast-2", "format", "utils", "format-rt.R"))

# Format forecast ---------------------------------------------------------
purrr::walk2(forecast_dates, submission_dates, format_rt)
