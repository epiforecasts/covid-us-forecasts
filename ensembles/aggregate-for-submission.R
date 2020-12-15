# Packages ----------------------------------------------------------------
library(data.table)
library(here)

# Target -----------------------------------------------------------------
forecast_date <- readRDS(here("data", "target_date.rds"))

# Load target forecasts ---------------------------------------------------
load_forecast <- function(model, date = forecast_date) {
  fread(here("ensembles", "data", model, paste0(date, ".csv")))
}
unweighted <- load_forecast("unweighted")

# Combine and save -------------------------------------------------------
forecasts <- unweighted
fwrite(forecasts, here("submissions", "ensembles", paste0(forecast_date, ".csv")))
