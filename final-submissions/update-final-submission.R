# Final submission
# 
# Set variable for which ensemble (QRA or QA (mean))
ensemble = "qa"

# Get ensemble
submit_ensemble <- readr::read_csv(here::here("ensembling", "quantile-average", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-", ensemble, ".csv")))

# Filter to states with 100+ deaths in last week
source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 100, last_week = 1)

submit_ensemble <- dplyr::filter(submit_ensemble, location %in% c(keep_states$state_code, "US"))

# Set forecast date
forecast_date <- unique(dplyr::pull(submit_ensemble, forecast_date))
  
# Save in final-submissions

readr::write_csv(submit_ensemble,
                 here::here("final-submissions", "death-forecast",
                            paste0(forecast_date, "-epiforecasts-ensemble1.csv")))
