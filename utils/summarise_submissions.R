summarise_submissions <- function(forecasts) {
  forecasts <- forecasts[, .(date = target_end_date, location, quantile, value, model, target)]
  forecasts <- forecasts[grepl("inc", target)]
  
  state_codes <- readRDS(here("data", "state_codes.rds"))
  forecasts <- forecasts[state_codes, on = "location"]
  
  forecasts <- dcast(forecasts, ... ~ quantile, value.var = "value")
  forecasts <- forecasts[!is.na(date)]
  forecasts <- forecasts[, c("date", "location", "model", "state", "NA", "0.025", "0.25", "0.75", "0.975")]
  setnames(forecasts, "NA", "value")
}