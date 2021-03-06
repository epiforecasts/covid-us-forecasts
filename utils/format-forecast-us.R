# Packages ----------------------------------------------------------------
library(EpiNow2)
library(data.table)
library(lubridate)
library(here)
source(here("utils/dates-to-epiweek.R"))
source(here("utils/get-us-data.R"))

# forecast_date -> date forecast was made
format_forecast_us <- function(forecasts, shrink_per = 0,
                               forecast_date = NULL, submission_date = NULL,
                               max_value = 1e7){
   
  forecasts <- as.data.table(forecasts)
   
  # Filter to full epiweeks
  forecasts <- dates_to_epiweek(forecasts)
  forecasts <- forecasts[epiweek_full == TRUE]
  forecasts <- forecasts[,  epiweek := lubridate::epiweek(date)]
  
  # Aggregate to weekly incidence
  weekly_forecasts_inc <- forecasts[,.(deaths = sum(deaths, na.rm = TRUE), target_end_date = max(date)), 
                                  by = .(epiweek, state, sample)]
  
  weekly_forecasts_inc <- weekly_forecasts_inc[order(deaths)][,
                                    .SD[round(.N * shrink_per, 0):round(.N * (1 - shrink_per), 0)],
                                    by = .(epiweek, state)]

  # Take quantiles
  weekly_forecasts <- weekly_forecasts_inc[, 
                            .(value = quantile(deaths, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=T),
                             quantile = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), 
                             target_end_date = max(target_end_date)), 
                         by = .(state, epiweek)][order(state, epiweek)]
   
  # Add necessary columns
  # dates and types
  forecasts_format <- weekly_forecasts[, `:=` (forecast_date = forecast_date,
                                  submission_date = submission_date,
                                  type = "quantile")]
                          
   # Add point forecasts
  forecasts_point <- forecasts_format[quantile == 0.5]
  forecasts_point <- forecasts_point[, `:=` (type = "point", quantile = NA)]
  forecasts_format <- rbind(forecasts_format, forecasts_point)
  
   # state codes
  state_codes <- readRDS(here::here("data/state_codes.rds"))
  forecasts_format <- forecasts_format[state_codes, on = "state", nomatch = 0]
  
  # assign horizon
  forecasts_format <- forecasts_format[target_end_date > forecast_date]
  forecasts_format <- forecasts_format[, horizon := 1 + as.numeric(target_end_date - min(target_end_date)) / 7]
  forecasts_format <- forecasts_format[, target := paste0(horizon, " wk ahead inc death")]
  data.table::setorder(forecasts_format, location, horizon, quantile)
       
  # drop unnecessary columns
  forecasts_format <- forecasts_format[, !c("horizon", "epiweek", "state")]
  
   # Set column order
  forecasts_format <- data.table::setcolorder(forecasts_format,
                                       c("forecast_date", "submission_date", 
                                         "target", "target_end_date", "location", 
                                         "type", "quantile", "value"))
  forecasts_format <- forecasts_format[, value := ifelse(value > max_value, max_value, value)]
  return(forecasts_format)
}
