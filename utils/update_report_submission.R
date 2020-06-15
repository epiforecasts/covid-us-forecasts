# Packages ----------------------------------------------------------------

require(here)
require(stringr)
require(dplyr)
require(purrr)

# Control parameters ------------------------------------------------------

forecast_date <- Sys.Date()
forecast_dir <- here::here("rt-forecast")  # Assumes forecasts are in national and regional subfolders

# Find forecasts ----------------------------------------------------------

forecasts <- c(list.dirs(file.path(forecast_dir, "state"), recursive = FALSE),
               list.dirs(file.path(forecast_dir, "national"), recursive = FALSE))
names(forecasts) <- forecasts %>% 
  stringr::str_remove(file.path(forecast_dir, "state/")) %>% 
  stringr::str_remove(file.path(forecast_dir, "national/"))

# Extract forecasts -------------------------------------------------------

source(here::here("utils/format_submission.R"))

region_forecasts <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_forecast(loc = .x, loc_name = .y, 
                                                      forecast_date = forecast_date,
                                                      forecast_adjustment = 11 + 5))

# Save forecast -----------------------------------------------------------

readr::write_csv(region_forecasts, 
                 paste0("death-forecast/epiforecasts-ensemble1/", forecast_date, "-epiforecasts-ensemble1.csv"))
