# Check incident forecast adds to cumulative
source("utils/load-submissions-function.R")
source("utils/get-us-data.R")
state_codes <- readRDS("utils/state_codes.rds")

# get cumulative data
cumulative <- get_us_deaths(data = "cumulative") %>%
  dplyr::ungroup() %>%
  dplyr::filter(date == forecast_date) %>%
  dplyr::add_row(state="US", deaths = sum(.$deaths), date = forecast_date) %>%
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
                value_inc, value_cum, cum_to_inc, diff_inc_cum)

us_summary <- dplyr::group_by(us_join, model) %>%
  dplyr::summarise(diff_mean = mean(diff_inc_cum),
                   .groups = "drop")

if(!mean(us_join$diff_inc_cum) == 0){
  warning("Incident and cumulative forecasts don't match")
}




