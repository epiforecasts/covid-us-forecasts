
# Packages ----------------------------------------------------------------
require(EpiNow2)
require(data.table)
require(lubridate)
source(here::here("utils/dates-to-epiweek.R"))
source(here::here("utils/get-us-data.R"))

# forecast_date -> date forecast was made
format_forecast_us <- function(forecasts, 
                               forecast_date = NULL, submission_date = NULL){
  
  # Filter to full epiweeks
  forecasts <- dates_to_epiweek(forecasts)
  forecasts <- forecasts[epiweek_full == TRUE]
  
  # Take quantiles
  forecasts <- forecasts[, .(value = quantile(deaths, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), na.rm=T),
                               quantile = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
                               epiweek = lubridate::epiweek(date)), 
                               by = .(state, date)][order(state, date)]

  # Aggregate to weekly incidence
  weekly_forecasts_inc <- forecasts[, keyby = .(epiweek, quantile, state),
                                .(value = sum(value),
                                  target_end_date = max(date),
                                  target_value = "inc")]
  
  # Add cumulative from last week
  cumulative_state <- data.table(get_us_deaths(data = "cumulative"))
  cumulative_state <- cumulative_state[date == max(cumulative_state$date), 
                                               .(state, deaths)]
  cumulative_national <- cumulative_state[, .(deaths = sum(deaths), 
                                              state = "US")]
  cumulative <- rbind(cumulative_state, cumulative_national)
  cumulative <- cumulative[state %chin% weekly_forecasts_inc$state]
  
  weekly_forecasts_cum <- weekly_forecasts_inc[cumulative, on = "state"]
  weekly_forecasts_cum <- weekly_forecasts_cum[, `:=`(value = value + deaths,
                                                      target_value = "cum",
                                                      deaths = NULL)]
  # Bind incident and cumulative
  weekly_forecasts <- rbind(weekly_forecasts_inc, weekly_forecasts_cum)
  
  # Add necessary columns
  # dates and types
  forecasts_format <- weekly_forecasts[, `:=` (forecast_date = forecast_date,
                                  submission_date = submission_date,
                                  type = "quantile",
                                  horizon = 1 + epiweek - lubridate::epiweek(submission_date))][
                                  , `:=` (target = paste0(horizon, " wk ahead ", target_value, " death"))]
                          
rm(weekly_forecasts, weekly_forecasts_cum, weekly_forecasts_inc)
  
   # Add point forecasts
  forecasts_point <- forecasts_format[quantile == 0.5]
  forecasts_point <- forecasts_point[, `:=` (type = "point", quantile = NA)]
  
  forecasts_format <- rbind(forecasts_format, forecasts_point)
  
   # state codes
  state_codes <- readRDS(here::here("utils/state_codes.rds"))
  forecasts_format <- forecasts_format[state_codes, on = "state", nomatch = 0]
  
  # drop unnecessary columns
  forecasts_format <- forecasts_format[, !c("horizon", "target_value", "epiweek", "state")]
                                  
  # Set column order
  forecasts_format <- data.table::setcolorder(forecasts_format,
                                       c("forecast_date", "submission_date", 
                                         "target", "target_end_date", "location", 
                                         "type", "quantile", "value"))
  
  forecasts_format <- forecasts_format[target_end_date > forecast_date]
  
  return(forecasts_format)
}
