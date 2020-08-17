
# past forecasts ---------------------------------------------------
all_dates <- readRDS(here::here("utils", "all_dates.rds"))
forecast_dates <- sort(as.vector(all_dates$rt_forecasts), decreasing = TRUE)
forecast_dates <- forecast_dates[2:5]

# Get format function -----------------------------------------------------

source(here::here("rt-forecast-2", "format", "utils", "format-rt.R"))

# Format forecast ---------------------------------------------------------

purrr::walk(forecast_dates, format_rt)
