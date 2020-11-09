
# past forecasts ---------------------------------------------------
# Get last 4 dates from a previously run Rt model
submission_dates <- dir("rt-forecast-2/output/original/submission-files/dated")
submission_dates <- gsub(pattern = "-rt-2-forecast\\.csv", replacement = "", x = submission_dates)
submission_dates <- as.Date(sort(submission_dates, decreasing = TRUE))[1:4]

forecast_dates <- submission_dates

# Get format function -----------------------------------------------------
source(here::here("rt-forecast-2", "format", "utils", "format-rt.R"))


# Select models to format -------------------------------------------------

# # Format all models:
# source("utils/meta-model-list.R")
# rt_models <- names(model_list$single_models)[grepl("rt", names(model_list$single_models))] 

# Format one model:
models <- list("backcalc")

# Format forecast ---------------------------------------------------------
purrr::walk2(.x = forecast_dates, 
             .y = submission_dates, 
             ~ format_rt(forecast_date = .x,
                         submission_date = .y,
                         rt_models = models))
