# Packages ----------------------------------------------------------------
library(here)
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)
state_locations <- readRDS(here("data", "state_codes.rds"))

# Error catching ----------------------------------------------------------
error_message <- list()

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds")))
  
# Choose submission -------------------------------------------------------
source(here("utils", "load_submissions.R"))
submission <- load_submissions(target_date, "ensembles", summarise = FALSE)

# Check distance between ensemble forecasts -------------------------------
single_models <- load_submissions(target_date, "all-models", summarise = FALSE)
distance <- submission %>%
  filter(model == "median") %>%
  select(target, location, type, quantile, ensemble_median = value) %>%
  left_join(state_locations, by = "location")

distance <- left_join(single_models, distance, 
                        by = c("target", "location", "type", "quantile")) %>%
  mutate(ensemble_distance = value - ensemble_median,
         relative_ensemble = ifelse(ensemble_median > 0, 
                                    value / ensemble_median,
                                    ifelse(ensemble_distance < 50, # If median == 0, any forecast <50 is OK
                                           NA, ensemble_distance)))

# in these states, models diverge: a model is on average < 1/5 or > 5x the median ensemble
central_diverge_locations <- distance %>%
  filter(quantile %in% c(0.25, 0.5, 0.75)) %>%
  group_by(target, location, state, model) %>%
  summarise(n = n(),
            mean_percent_distance = mean(relative_ensemble, na.rm = T),
            .groups = "drop") %>% 
  filter(mean_percent_distance > 5 | mean_percent_distance < 0.2) %>% 
  pull(state) %>% 
  unique() %>%
  sort()

error_message <- c(error_message,
                   list("Check diverging models: Following locations have models with central estimates >5x median:" = 
                          central_diverge_locations))

outer_diverge_locations <- distance %>%
  filter(quantile %in% c(0.1, 0.9)) %>%
  group_by(target, location, state, model) %>%
  summarise(n = n(),
            mean_percent_distance = mean(relative_ensemble, na.rm = T),
            .groups = "drop") %>% 
  filter(mean_percent_distance > 10 | mean_percent_distance < 0.1) %>% 
  pull(state) %>% 
  unique() %>%
  sort()
error_message <- c(error_message,
                   list("Check diverging models: Following locations have models with central estimates >10x median:" = 
                          outer_diverge_locations[!outer_diverge_locations %in% central_diverge_locations]))

# Use QRA by default ------------------------------------------------------
submission <- submission[(window == 4 & horizons == "4" & model == "QRA")]

# Convert -----------------------------------------------------------------
submission <- submission[, 
 c("window", "model", "horizons", "submission_date") := NULL]
submission <- submission[, value := as.integer(value)]

# # Replace some states with a single model ---------------------------------
if (file.exists(here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))) {
  swap <- readRDS(here("submissions", "utils", paste0(target_date, "-swap-ensemble.rds")))
  
  for (swap_model in names(swap)) {
    swap_names <- data.frame("state" = swap[swap_model][[1]])
    swap_locs <- merge(swap_names, state_locations, by = "state")$location
    if ("US" %in% swap_names$state) {
      swap_locs <- c(swap_locs, "US")
    }
    all_ensembles <- load_submissions(target_date, "ensembles", summarise = FALSE)
    all_models <- load_submissions(target_date, "all-models", summarise = FALSE)
    alt_subs <- rbind(all_ensembles, all_models, fill = TRUE)
    alt_subs <- alt_subs[(model == swap_model & location %in% swap_locs)]
    keep_cols <- names(submission)
    alt_subs <- alt_subs[, ..keep_cols]
    submission <- submission[(!location %in% swap_locs)]
    submission <- rbind(submission, alt_subs)
  }
}

# Check for crossing quantiles --------------------------------------------
cross_submission <- copy(submission)[!is.na(quantile), 
  .(quantile, value, crossing = value < shift(value, fill = 0)),
  by = .(forecast_date, target, target_end_date, location, type)]

crossing_locations <- copy(cross_submission)[, 
  .(crossing = as.numeric(sum(crossing))), by = .(location)]
crossing_locations <- unique(crossing_locations[crossing > 0, ]$location)

if (length(crossing_locations) > 0) {
  error_message <- c(error_message,
    list("Auto-corrected: Following locations contained crossing quantiles:" = 
           crossing_locations))
  
  while (sum(cross_submission$crossing) > 0) {
    cross_submission <- cross_submission[, 
      value := ifelse(crossing, shift(value, fill = 0), value),
      by = .(forecast_date, target, target_end_date, location, type)]
    cross_submission <- cross_submission[, 
      .(quantile, value, crossing = value < shift(value, fill = 0)),
      by = .(forecast_date, target, target_end_date, location, type)]
  }
  cross_submission <- cross_submission[, crossing := NULL]
  point_submission <- cross_submission[quantile == 0.5][, 
    `:=`(type = "point", quantile = NA)]
  submission <- rbindlist(list(point_submission, cross_submission))
  setorder(submission, location, target, target_end_date, type)
}
  
# Add cumulative forecast -------------------------------------------------
# get cumulative data
source(here("utils", "get-us-data.R"))
cumulative <- setDT(get_us_data(data = "deaths",
                                      include_national = TRUE,
                                      incident = FALSE))
cumulative <- setnames(cumulative, "value", "deaths")

cumulative <- 
  cumulative[date == min(as.Date(submission$target_end_date)) - weeks(1),
                   .(state, deaths)]

# link with state codes 
state_codes <- readRDS(here("data", "state_codes.rds"))
cumulative <- cumulative[state_codes, on = "state", nomatch = 0]
cumulative <- cumulative[location %chin% submission$location]
cumulative <- cumulative[, state := NULL]

cum_submission <- copy(submission)[cumulative, on = "location"]

# Make forecast values cumulative
cum_submission <- cum_submission[, "value" := cumsum(value), 
                                 by = c("location", "type", "quantile")]  

cum_submission <- cum_submission[,
  `:=`(value = value + deaths, 
       target = str_replace_all(target, " inc ", " cum "), 
       deaths = NULL)]
# link inc and cum submissions
submission <- rbindlist(list(submission, cum_submission))

# Checks ------------------------------------------------------------------
# adjust to total population if greater than
state_pop <- fread(here("data", "state_pop.csv"))
state_pop <- state_pop[, .(location = state_code, pop = tot_pop)]
submission <- merge(submission, state_pop, by = "location")
submission <- submission[pop < value,
  `:=`(value = pop - 1, value_exceeds_pop = 1)]

if (sum(submission$value_exceeds_pop, na.rm = TRUE) > 0) {
  error_message <- c(error_message,
                     list("Auto-corrected: Forecast values exceeded total population in:" =
                            submission[value_exceeds_pop == 1, location]))
}
submission[, c("value_exceeds_pop", "pop") := NULL]

# check for NAs
na_submissions <- submission[is.na(value), location]
submission <- submission[!is.na(value)]
if (length(na_submissions) > 0) {
  error_message <- c(error_message,
                     list("Forecast values are NA in:" = na_submissions))
}

# check for identically 0
if (sum(submission$value) == 0) {
  error_message <- c(error_message,
                     list("Forecast is zero for all submission targets and values"))
  stop("Forecast is zero for all submission targets and values")
}

if (length(error_message) == 0) {
  error_message <- list("Checks completed:" = "All checks passed")
}

# Save submission ---------------------------------------------------------
fwrite(submission, here("submissions", "submitted",
                        paste0(target_date, "-epiforecasts-ensemble1.csv")))

saveRDS(error_message, here("submissions", "utils", paste0(target_date, "-errors.rds")))

        