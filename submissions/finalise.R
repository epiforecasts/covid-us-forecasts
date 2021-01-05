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

# check non-crossing quantiles
submission <- submission %>%
  dplyr::group_by(target, location, type) %>%
  dplyr::mutate(quantile_incr = ifelse(value <= lag(value), lag(value), value),
                quantile_incr = ifelse(quantile_incr <= lag(quantile_incr), lag(quantile_incr), quantile_incr),
                quantile_incr = ifelse(quantile_incr <= lag(quantile_incr), lag(quantile_incr), quantile_incr),
                quantile_incr = ifelse(quantile_incr <= lag(quantile_incr), lag(quantile_incr), quantile_incr),
                quantile_incr = ifelse(quantile_incr <= lag(quantile_incr), lag(quantile_incr), quantile_incr),
                value = ifelse(is.na(quantile_incr), value, quantile_incr),
                quantile_incr = NULL) %>%
  dplyr::ungroup()

# if there are still more than 5 quantiles that cross then maybe we discard ?!
quantile_check <- submission %>%
  dplyr::group_by(target, location, type) %>%
  dplyr::mutate(increase = value - lag(value)) %>%
  dplyr::filter(increase < 0) %>%
  dplyr::pull(location) %>%
  unique()

submission <- submission %>%
  dplyr::filter(!location %in% quantile_check)

submission <- as.data.table(submission)



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
submission <- submission[pop < value, `:=`(value = pop, value_exceeds_pop = 1)]

if (sum(submission$value_exceeds_pop, na.rm = TRUE) > 0) {
  warning("Forecast values exceed total population")
  print(submission[value_exceeds_pop == 1])
}
submission[, value_exceeds_pop := NULL]
submission[, pop := NULL]

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
