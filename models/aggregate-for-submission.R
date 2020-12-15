# Packages ----------------------------------------------------------------
library(data.table)
library(here)

# Target -----------------------------------------------------------------
forecast_date <- readRDS(here("data", "target_date.rds"))

# Load target forecasts ---------------------------------------------------
load_forecast <- function(model, date = forecast_date) {
  fread(here("models",  model, "data", "submission", "dated", paste0(date, ".csv")))
}

rt <- load_forecast("rt")[, model := "Rt"]
timeseries <-  load_forecast("timeseries")[, model := "Timeseries"]
conv <-  load_forecast("deaths-conv-cases")[, model := "Case convolution"]

# Combine and save -------------------------------------------------------
forecasts <- rbindlist(list(rt, timeseries, conv), use.names = TRUE)
fwrite(forecasts, here("submissions", "all-models", paste0(forecast_date, ".csv")))
