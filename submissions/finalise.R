# Packages ----------------------------------------------------------------
library(here)
library(data.table)
library(lubridate)
library(stringr)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds"))) 

# Choose submission -------------------------------------------------------
submission <- fread(here("submissions", "ensembles", paste0(target_date, ".csv")))
submission <- submission[(window == 8 & horizons == "4")]
submission <- submission[model == "QRA (weighted quantiles)"]

# Convert -----------------------------------------------------------------
submission <- submission[, c("window", "model", "horizons", "submission_date") := NULL]

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
submission <- submission[pop < value, `:=`(value = pop, value_exceeds_pop = 1)]

if (sum(submission$value_exceeds_pop, na.rm = TRUE) > 0) {
  warning("Forecast values exceed total population")
  print(submission[value_exceeds_pop == 1])
}
submission[, value_exceeds_pop := NULL]


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
