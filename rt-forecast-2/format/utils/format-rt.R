# Format output from Epinow2
# Packages ----------------------------------------------------------------
require(EpiNow2)
require(here)
require(stringr)
require(dplyr)
require(purrr)
require(ggplot2)
require(cowplot)
require(data.table)


format_rt <- function(forecast_date, submission_date, include_latest = FALSE) {
  
  rt_models <- c("original", "fixed_rt")
  
  for(i in rt_models){
    
    # Set directory ------------------------------------------------------
    
    forecast_dir <- here::here("rt-forecast-2/forecast/deaths_forecast", i)
    output_dir <- here::here("rt-forecast-2/output", i)
    
    # Load forecasts ----------------------------------------------------------
    
    forecasts_raw <- suppressWarnings(
      EpiNow2::get_regional_results(results_dir = file.path(forecast_dir, "state"),
                                                   date = lubridate::ymd(forecast_date) - lubridate::days(1),
                                                   forecast = TRUE)$estimated_reported_cases$samples
    )
    
    # Format samples ----------------------------------------------------------
    data.table::setnames(forecasts_raw, old = c("region", "cases"), new = c("state", "deaths"))
    
    forecasts_samples <- forecasts_raw[, .(sample = sample,
                                           deaths = deaths,
                                           target_end_date = date,
                                           forecast_date = forecast_date,
                                           model = paste0("Rt2-", i),
                                           location = state)]
    
    forecasts_samples <- forecasts_samples[target_end_date > submission_date]
    
    # Save samples
    saveRDS(forecasts_samples, 
            paste0(output_dir, "/samples/", submission_date, "-rt-forecast-samples.rds"))
    
    
    # Format forecasts --------------------------------------------------------
    source(here::here("rt-forecast-2/format/utils/format-forecast-us.R"))
    
    formatted_forecasts <- format_forecast_us(forecasts = forecasts_raw, 
                                              forecast_date = forecast_date, 
                                              submission_date = submission_date)
    
    
    # Save forecast -----------------------------------------------------------
    
    readr::write_csv(formatted_forecasts, 
                     paste0(output_dir, "/submission-files/dated/", submission_date, "-rt-2-forecast.csv"))
    
    if (include_latest) {
      readr::write_csv(formatted_forecasts, 
                       paste0(output_dir, "/submission-files/latest.csv"))
    }
    
  }
  
  return(invisible(NULL))
}


