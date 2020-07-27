# Forecasting and submission dates
library(magrittr)

get_dates <- function(ts_do_forecast_date, 
                      ts_doc_forecast_date, 
                      rt_forecast_date,
                      submission_date,
                      save = FALSE){
  # 
  # Previous forecasting weeks:
  # 
  # all_dates <- tibble::tibble(
  #                     ts_deaths_only_forecasts = c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20"),
  #                     ts_deaths_on_cases_forecasts = c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20"),
  #                     rt_forecasts = c("2020-06-15", "2020-06-22", "2020-06-28", "2020-07-05", "2020-07-13", "2020-07-19"),
  #                     submissions = c("2020-06-15", "2020-06-22", "2020-06-29", "2020-07-06", "2020-07-13", "2020-07-20")) %>%
  #   dplyr::mutate(ts_deaths_only_forecasts = as.Date(ts_deaths_only_forecasts),
  #                 ts_deaths_on_cases_forecasts = as.Date(ts_deaths_on_cases_forecasts),
  #                 rt_forecasts = as.Date(rt_forecasts),
  #                 submissions = as.Date(submissions))

  all_dates <- readRDS(here::here("utils", "all_dates.rds")) %>%
    dplyr::add_row(ts_deaths_only_forecasts = ts_do_forecast_date,
                   ts_deaths_on_cases_forecasts = ts_doc_forecast_date,
                   rt_forecasts = rt_forecast_date, 
                   submissions = submission_date) %>%
    dplyr::distinct(.keep_all = TRUE)

  if(save){
  saveRDS(all_dates, here::here("utils", "all_dates.rds"))
  }
  
  return(all_dates)
  
}
  
