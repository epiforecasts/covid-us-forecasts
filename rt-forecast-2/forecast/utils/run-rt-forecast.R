
require(lubridate, quietly = TRUE)
# Set up running a single Rt forecast -------------------------------------

run_rt_forecast <- function(deaths, submission_date, rerun = FALSE) {
  
  if (missing(submission_date)) {
    rerun <- TRUE
  }
  # Set up directories for models -------------------------------------------
  models <- list("original", "fixed_rt")
  
  targets <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/state"))
  names(targets) <- models
  
  summary <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/summary"))
  names(summary) <- models
  
  
  if (!rerun) {
    targets_present <- purrr::map_lgl(targets, ~ dir.exists(file.path(., "US",
                                                                      lubridate::ymd(submission_date) -
                                                                        lubridate::days(1))))
    
    models <- models[!targets_present]
  }
  

  # Format for epinow2 ------------------------------------------------------
  
  if (!missing(submission_date)) {
    deaths <- dplyr::filter(deaths, date < submission_date)
  }
  
  deaths <- setDT(deaths)
  deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(12))], by = region]
  
  data.table::setorder(deaths, date)
  
  
  
  # Set up common settings --------------------------------------------------
  
  std_regional_epinow <- purrr::partial(regional_epinow, 
                                        reported_cases = deaths,
                                        generation_time = generation_time,
                                        delays = list(incubation_period, reporting_delay),
                                        horizon = 30,
                                        samples = 2000,
                                        warmup = 500,
                                        burn_in = 14,
                                        adapt_delta = 0.98,
                                        cores = no_cores,
                                        chains = ifelse(no_cores <= 2, 2, no_cores),
                                        return_estimates = FALSE, verbose = FALSE
  )
  # Run Rt - ORIGINAL -------------------------------------------------------
  if (models %in% "original") {
    std_regional_epinow(target_folder = targets[["original"]],
                        summary_dir = summary[["original"]])
  }

  # Run Rt - FIXED RT --------------------------------------------------
  
  if (models %in% "fixed_rt") {
    std_regional_epinow(target_folder = targets[["fixed_rt"]],
                        summary_dir = summary[["fixed_rt"]],
                        fixed_future_rt = TRUE)
  }

  # Add more models here ----------------------------------------------------
  
  return(invisible(NULL))
}

