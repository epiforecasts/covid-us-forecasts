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


format_rt <- function(forecast_date, submission_date, include_latest = FALSE,
                      sample_range = 0.4) {
  
  # Get names Rt models
  source("utils/meta-model-list.R")
  rt_models <- names(model_list$single_models)[grepl("rt", names(model_list$single_models))] 
  
  i <- rt_models[1]
  
  for(i in rt_models){
    
    # Set directory ------------------------------------------------------
    
    i <- gsub("rt2_", "", i)
    message("Formating: ", i, " on the ", forecast_date)
    
    forecast_dir <- here::here("rt-forecast-2/forecast/deaths_forecast", i)
    output_dir <- here::here("rt-forecast-2/output", i)
    
    

    # Create results directories if not present --------------------------------
    create_dir <- function(new_dir) {
      if (!dir.exists(paste0(output_dir, "/", new_dir))) {
        dir.create(paste0(output_dir, "/", new_dir), recursive = TRUE)
      }
      
      return(invisible(NULL))
    }

    create_dir("samples")
    create_dir("submission-files")
    create_dir("submission-files/dated")
    
    # Load forecasts ----------------------------------------------------------
    
    # Try yesterday's date for latest Rt forecast file
    forecasts_raw <- suppressWarnings(
      EpiNow2::get_regional_results(results_dir = file.path(forecast_dir, "state"),
                                                   date = lubridate::ymd(forecast_date),
                                                   forecast = TRUE)$estimated_reported_cases$samples
    )
    
    # If nothing returns, try today's date
    if(length(forecasts_raw) == 0){
      forecasts_raw <- suppressWarnings(
        EpiNow2::get_regional_results(results_dir = file.path(forecast_dir, "state"),
                                      date = lubridate::ymd(forecast_date),
                                      forecast = TRUE)$estimated_reported_cases$samples
      )
      
      if(length(forecasts_raw) == 0){
        warning("<format-rt.R> Latest Rt forecasts are not found. Check forecast_date and directory paths")
        }
    }
    
     
   
     # Shrink samples ----------------------------------------------------------

    shrink_per <- (1 - sample_range) / 2
    
    forecasts_raw <- forecasts_raw[order(cases)][, 
                                   .SD[round(.N * shrink_per, 0):round(.N * (1 - shrink_per), 0)],
                                         by = .(region, date)]
    
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


