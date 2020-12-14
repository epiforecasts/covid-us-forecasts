# Final submission
# 
# Set variable for which ensemble (QRA or QA (mean))
ensemble_dir = "qra-state-ensemble" # c("qra-state-ensemble" "qra-ensemble", "quantile-average")

# Get ensemble
submit_ensemble <- suppressMessages(readr::read_csv(here::here("ensembling", ensemble_dir, "submission-files",
                                            paste0("latest.csv"))))

# Filter to states with minimum deaths in last week
source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 5, last_week = 1)

submit_ensemble <- dplyr::filter(submit_ensemble, location %in% c(keep_states$state_code, "US"))

# Set forecast date
forecast_date <- unique(dplyr::pull(submit_ensemble, forecast_date))

# Filter to forecasts within Rt forecast
# rt_max_date <- suppressMessages(readr::read_csv(here::here("rt-forecast/submission-files/latest.csv"))) %>%
#   dplyr::pull(target_end_date) %>%
#   unique() %>%
#   max()


submit_ensemble <- dplyr::filter(submit_ensemble, (target_end_date - submission_date) <= 30) %>%
  dplyr::select(-submission_date)

# 3. Incident and cumulative add up ------------------------------------------
#  - check each model to find the issue
# Check incident forecast adds to cumulative
source("utils/load-submissions-function.R")
source("utils/get-us-data.R")
state_codes <- readRDS("utils/state_codes.rds")
# get cumulative data
cumulative <- get_us_deaths(data = "cumulative") %>%
  dplyr::ungroup() %>%
  dplyr::filter(date == forecast_date-2) %>%
  dplyr::add_row(state="US", deaths = sum(.$deaths, na.rm = TRUE), date = forecast_date-2) %>%
  dplyr::left_join(state_codes, by = "state")
# Check each model to find the issue
forecasts <- load_submission_files(dates = "all", num_last = 1, models = "single") %>%
  dplyr::filter(location == "US")
us_inc <- dplyr::filter(forecasts, grepl("inc", forecasts$target))
us_cum <- dplyr::filter(forecasts, grepl("cum", forecasts$target)) %>%
  dplyr::group_by(location, quantile, type) %>%
  dplyr::mutate(cum_to_inc = value - dplyr::lag(value, 1)) %>%
  dplyr::ungroup()

us_join <- dplyr::left_join(us_inc, us_cum, by = c("model", 
                                                   "location", "target_end_date", 
                                                   "type", "quantile")) %>%
  dplyr::left_join(cumulative, by = c("location")) %>%
  dplyr::rename(value_inc = value.x, value_cum = value.y) %>%
  dplyr::mutate(cum_to_inc = ifelse(is.na(cum_to_inc), value_cum - deaths, cum_to_inc),
                diff_inc_cum = value_inc - cum_to_inc) %>%
  dplyr::select(model,
                location, state, target_end_date, type, quantile, deaths, 
                value_inc, value_cum, cum_to_inc, diff_inc_cum) %>%
  dplyr::group_by(model, target_end_date) %>%
  dplyr::summarise(diff_inc_cum = mean(diff_inc_cum),
                   .groups = "drop")
if(!mean(us_join$diff_inc_cum) == 0){
  warning("Incident and cumulative forecasts don't match. 
          Re-writing cumulative forecasts for the submission ensemble")
  
  incident_forecast <- dplyr::filter(submit_ensemble,
                                     grepl("wk ahead inc", target))
  
  cumulative_data <- get_us_deaths(data = "cumulative")
  cumulative_deaths <- cumulative_data %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == min(max(date), forecast_date)) %>%
    dplyr::add_row(state="US", deaths = sum(.$deaths, na.rm = TRUE), date = forecast_date) %>%
    dplyr::left_join(state_codes, by = "state")
  
  cumulative_forecast <- incident_forecast %>%
    dplyr::left_join(dplyr::select(cumulative_deaths, location, deaths),
                     by = "location") %>%
    dplyr::group_by(location, quantile, type) %>%
    dplyr::mutate(value = cumsum(value),
                  value = value + deaths,
                  target = stringr::str_replace_all(target, "inc", "cum")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-deaths)
  
  # Check this also adds up
  # join <- dplyr::left_join(incident_forecast, cumulative_forecast,
  #                          by = c("forecast_date", "target_end_date", 
  #                          "location", "type", "quantile")) %>%
  #   dplyr::rename(value_inc = value.x, value_cum = value.y) %>%
  #   dplyr::group_by(location, type, quantile) %>%
  #   dplyr::mutate(cum_to_inc = (value_cum - dplyr::lag(value_cum))-value_inc)
  
  submit_ensemble <- dplyr::bind_rows(incident_forecast, cumulative_forecast)

}

# Checks ------------------------------------------------------------------


# 1. Check population limit
pop_check <- dplyr::left_join(submit_ensemble, readr::read_csv("utils/state_pop_totals.csv"), 
                              by = c("location" = "state_code")) %>%
  dplyr::mutate(pop_check = ifelse(value > tot_pop, FALSE, TRUE)) %>%
  dplyr::filter(pop_check == FALSE) %>%
  dplyr::pull(location) %>%
  unique()

# 2. Check for NA values
na_check <- submit_ensemble %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::pull(location)

# Filter failing checks ---------------------------------------------------
if((length(na_check) | length(pop_check)) > 0){
    message("Excluding states failing checks:")
    print(dplyr::filter(state_codes, location %in% c(pop_check, na_check)) %>%
                dplyr::pull(state))
}

submit_ensemble <- submit_ensemble %>%
  dplyr::filter(!location %in% pop_check & 
                  !location %in% na_check)
                       
# Save in final-submissions

readr::write_csv(submit_ensemble,
                 here::here("final-submissions", "death-forecast",
                            paste0(unique(submit_ensemble$forecast_date), 
                                   "-epiforecasts-ensemble1.csv")))
