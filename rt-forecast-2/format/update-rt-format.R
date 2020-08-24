
# Get format function -----------------------------------------------------

source(here::here("rt-forecast-2", "format", "utils", "format-rt.R"))

# Get latest submission date ---------------------------------------------
# Latest forecast
source(here::here("utils", "current-forecast-submission-date.R"))

# Format forecast ---------------------------------------------------------

format_rt(forecast_date, forecast_date, include_latest = TRUE)
