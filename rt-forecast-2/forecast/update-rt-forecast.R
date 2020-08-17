# Load in Rt forecast data ------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "get-default-rt-data.R"))

# Get rt forecasts --------------------------------------------------------

source(here::here("rt-forecast-2", "forecast", "utils", "run-rt-forecast.R"))

# Run forecast for current data -------------------------------------------

run_rt_forecast(deaths)

