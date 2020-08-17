# Set dates ---------------------------------------------------------------

# Format past forecasts ---------------------------------------------------
all_dates <- readRDS(here::here("utils", "all_dates.rds"))
forecast_dates <- sort(as.vector(all_dates$submissions), decreasing = TRUE)
forecast_dates <- forecast_dates[2:5]

# Format forecast ---------------------------------------------------------

purrr::walk(forecast_dates, format_rt)
