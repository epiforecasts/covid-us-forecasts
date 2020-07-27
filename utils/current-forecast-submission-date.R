# update forecast and submission dates

forecast_date <- Sys.Date()
submission_date <- Sys.Date()+1 # update accordingly

source(here::here("utils", "forecast-submission-dates.R"))

all_dates <- get_dates(ts_do_forecast_date = forecast_date, 
                       ts_doc_forecast_date = forecast_date, 
                       rt_forecast_date = forecast_date,
                       submission_date = submission_date,
                       save = FALSE)
