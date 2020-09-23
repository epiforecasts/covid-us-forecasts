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


# Checks ------------------------------------------------------------------


# Check population limit
#   (back up copy of population totals saved in "data/pop_totals.csv")
pop <- readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>%
  dplyr::group_by(STATE) %>%
  dplyr::summarise(tot_pop = sum(POPESTIMATE2019), .groups = "drop") %>%
  dplyr::bind_rows(tibble::tibble("STATE" = c("US", 
                                              "66"), # Guam
                                  "tot_pop" = c(331002651,
                                                165768)))

pop_check <- dplyr::left_join(submit_ensemble, pop, by = c("location" = "STATE")) %>%
  dplyr::mutate(pop_check = ifelse(value > tot_pop, FALSE, TRUE)) %>%
  dplyr::filter(pop_check == FALSE) %>%
  dplyr::pull(location)

# Check for NA values
na_check <- submit_ensemble %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::pull(location)


# Filter failing checks ---------------------------------------------------

submit_ensemble <- submit_ensemble %>%
  dplyr::filter(!location %in% pop_check & 
                  !location %in% na_check)

                       
# Save in final-submissions

readr::write_csv(submit_ensemble,
                 here::here("final-submissions", "death-forecast",
                            paste0(forecast_date, "-epiforecasts-ensemble1.csv")))

