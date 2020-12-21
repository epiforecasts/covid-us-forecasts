# Packages ----------------------------------------------------------------
library(here)
library(data.table)
library(lubridate)
library(stringr)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds"))) 

# Choose submission -------------------------------------------------------
submission <- fread(here("submissions", "ensembles", paste0(target_date, ".csv")))
submission <- submission[model == "mean"]

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
# 1. Check population limit
pop_check <- dplyr::left_join(submission, readr::read_csv("data/state_pop_totals.csv"), 
                              by = c("location" = "state_code")) %>%
  dplyr::mutate(pop_check = ifelse(value > tot_pop, FALSE, TRUE)) %>%
  dplyr::filter(pop_check == FALSE) %>%
  dplyr::pull(location) %>%
  unique()
# 2. Check for NA values
na_check <- submission %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::pull(location)

# Filter failing checks 
if ((length(na_check) | length(pop_check)) > 0) {
  message("Excluding states failing checks:")
  print(dplyr::filter(state_codes, location %in% c(pop_check, na_check)) %>%
          dplyr::pull(state))
}

submission <- submission %>%
  dplyr::filter(!location %in% pop_check & 
                  !location %in% na_check)

# Save submission ---------------------------------------------------------
fwrite(submission, here("submissions", "submitted",
                        paste0(target_date, "-epiforecasts-ensemble1.csv")))
