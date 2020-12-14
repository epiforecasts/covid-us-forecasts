# update forecast and submission dates

forecast_date <- Sys.Date() - 1
submission_date <- Sys.Date() -1  # update accordingly

source(here::here("utils", "forecast-submission-dates.R"))
#
all_dates <- get_dates(ts_do_forecast_date = as.character(forecast_date),
                       ts_doc_forecast_date = as.character(forecast_date),
                       rt_forecast_date = as.character(forecast_date),
                       submission_date = as.character(submission_date),
                       save = TRUE)
