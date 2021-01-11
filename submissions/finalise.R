# Packages ----------------------------------------------------------------
library(here)
library(data.table)
library(lubridate)
library(stringr)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds"))) 
  
# Choose submission -------------------------------------------------------
submission <- fread(here("submissions", "ensembles", paste0(target_date, ".csv")))
submission <- submission[(window == 4 & horizons == "4")]
submission <- submission[model == "QRA"]

# Convert -----------------------------------------------------------------
submission <- submission[, c("window", "model", "horizons", "submission_date") := NULL]
submission <- submission[, value := as.integer(value)]

# Check for crossing quantiles --------------------------------------------
cross_submission <- copy(submission)[!is.na(quantile), .(quantile, value, crossing = value < shift(value, fill = 0)),
                                     by = .(forecast_date, target, target_end_date, location, type)]

crossing_locations <- copy(cross_submission)[, .(crossing = as.numeric(sum(crossing))), by = .(location)]
crossing_locations <- unique(crossing_locations[crossing > 0, ]$location)

if (length(crossing_locations) > 0) {
  warning("Following locations contain crossing quantiles (but will be corrected): ", paste(crossing_locations, collapse = ", "))
  while (sum(cross_submission$crossing) > 0) {
    cross_submission <- cross_submission[, value := ifelse(crossing, shift(value, fill = 0), value),
                                         by = .(forecast_date, target, target_end_date, location, type)]
    cross_submission <- cross_submission[, .(quantile, value, crossing = value < shift(value, fill = 0)),
                                         by = .(forecast_date, target, target_end_date, location, type)]
  }
  cross_submission <- cross_submission[, crossing := NULL]
  point_submission <- cross_submission[quantile == 0.5][, `:=`(type = "point", quantile = NA)]
  submission <- rbindlist(list(point_submission, cross_submission))
  setorder(submission, location, target, target_end_date, type)
}
  
# Add cumulative forecast -------------------------------------------------
# get cumulative data
source(here("utils", "get-us-data.R"))
cumulative_state <- setDT(get_us_deaths(data = "cumulative"))
cumulative_state <- cumulative_state[date == min(as.Date(submission$target_end_date)) - weeks(1), 
                                     .(state, deaths)]
cumulative_national <- cumulative_state[, .(deaths = sum(deaths), 
                                            state = "US")]
cumulative <- rbind(cumulative_state, cumulative_national)

# link with state codes 
state_codes <- readRDS(here("data", "state_codes.rds"))
cumulative <- cumulative[state_codes, on = "state", nomatch = 0]
cumulative <- cumulative[location %chin% submission$location]
cumulative <- cumulative[, state := NULL]

cum_submission <- copy(submission)[cumulative, on = "location"]

# Make forecast values cumulative
cum_submission <- cum_submission[, "value" := cumsum(value), 
                                 by = c("location", "type", "quantile")]  

cum_submission <- cum_submission[, `:=`(value = value + deaths,
                                        target = str_replace_all(target, " inc ", " cum "),
                                        deaths = NULL)]  
# link inc and cum submissions
submission <- rbindlist(list(submission, cum_submission))

# Checks ------------------------------------------------------------------

# adjust to total population if greater than
state_pop <- fread(here("data", "state_pop.csv"))
state_pop <- state_pop[, .(location = state_code, pop = tot_pop)]
submission <- merge(submission, state_pop, by = "location")
submission <- submission[pop < value, `:=`(value = pop - 1, value_exceeds_pop = 1)]

if (sum(submission$value_exceeds_pop, na.rm = TRUE) > 0) {
  warning("Forecast values exceed total population")
  print(submission[value_exceeds_pop == 1])
}
submission[, c("value_exceeds_pop", "pop") := NULL]

# check for NAs
na_submissions <- submission[is.na(value)]
submission <- submission[!is.na(value)]
if (nrow(na_submissions) > 0) {
  warning("Forecast values are NA")
  print(na_submissions)
}

# check for identically 0
if (sum(submission$value) == 0) {
  stop("Forecast is zero for all submission targets and values")
}
# Save submission ---------------------------------------------------------
fwrite(submission, here("submissions", "submitted",
                        paste0(target_date, "-epiforecasts-ensemble1.csv")))
