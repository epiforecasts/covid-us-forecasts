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
weighted <- load_forecast("weighted")

# Combine and save -------------------------------------------------------
forecasts <- rbindlist(list(unweighted, weighted), use.names = TRUE, fill = TRUE)
fwrite(forecasts, here("submissions", "ensembles", paste0(forecast_date, ".csv")))
