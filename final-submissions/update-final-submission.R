# Final submission
# 
# Set variable for which ensemble (QRA or QA (mean))
ensemble_dir = "quantile-average" # c("qra-state-ensemble" "qra-ensemble", "quantile-average")

# Get ensemble
submit_ensemble <- suppressMessages(readr::read_csv(here::here("ensembling", ensemble_dir, "submission-files",
                                            paste0("latest.csv"))))

# Filter to states with minimum deaths in last week
source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 5, last_week = 1)

submit_ensemble <- dplyr::filter(submit_ensemble, location %in% c(keep_states$state_code, "GM"))

# Set forecast date
forecast_date <- unique(dplyr::pull(submit_ensemble, forecast_date))

# Filter to forecasts within Rt forecast
# rt_max_date <- suppressMessages(readr::read_csv(here::here("rt-forecast/submission-files/latest.csv"))) %>%
#   dplyr::pull(target_end_date) %>%
#   unique() %>%
#   max()


submit_ensemble <- dplyr::filter(submit_ensemble, (target_end_date - submission_date) <= 30) %>%
  dplyr::select(-submission_date)

  
# Save in final-submissions

readr::write_csv(submit_ensemble,
                 here::here("final-submissions", "death-forecast",
                            paste0(forecast_date, "-epiforecasts-ensemble1.csv")))
