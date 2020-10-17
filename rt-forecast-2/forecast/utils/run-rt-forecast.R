
require(lubridate, quietly = TRUE)
# Set up running a single Rt forecast -------------------------------------

run_rt_forecast <- function(deaths, submission_date, rerun = FALSE) {
  
  if (missing(submission_date)) {
    rerun <- TRUE
  }
  # Set up directories for models -------------------------------------------
  models <- list("original", "fixed_future_rt", "fixed_rt", "no_delay")
  
  targets <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/state"))
  names(targets) <- models
  
  summary <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/summary"))
  names(summary) <- models
  
  
  if (!rerun) {
    targets_present <- purrr::map_lgl(targets, 
              ~ dir.exists(file.path(., "US", lubridate::ymd(submission_date) - lubridate::days(1))))
    
    models <- models[!targets_present]
  }
  
  if(length(models) == 0){
    return(invisible(NULL))
  }

  # Format for epinow2 ------------------------------------------------------
  
  if (!missing(submission_date)) {
    deaths <- dplyr::filter(deaths, date < submission_date)
  }
  
  deaths <- setDT(deaths)
  deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(8))], by = region]
  data.table::setorder(deaths, date)
  
  # Set up common settings --------------------------------------------------
  std_regional_epinow <- purrr::partial(regional_epinow, samples = 2000, horizon = 30, 
                                        generation_time = generation_time,
                                        stan_args = list(warmup = 500, cores = no_cores, 
                                                         control = list(adapt_delta = 0.95,
                                                                        max_treedepth = 15), 
                                                         chains = ifelse(no_cores <= 4, 4, no_cores)), 
                                        burn_in = 14, non_zero_points = 14,
                                        max_execution_time = 60 * 30, future = TRUE,
                                        output = c("region", "samples", "summary", "timing"))
  # Run Rt - ORIGINAL -------------------------------------------------------
   if ("original" %in% models) {
     std_regional_epinow(reported_cases = deaths,
                         target_folder = targets[["original"]],
                         summary_args = list(summary_dir = summary[["original"]],
                                             all_regions = FALSE),
                         logs = "rt-forecast-2/logs/original",
                         delays = list(incubation_period, reporting_delay))
   }

  # Run Rt - FIXED RT --------------------------------------------------

   if ("fixed_future_rt" %in% models) {
     std_regional_epinow(reported_cases = deaths,
                         target_folder = targets[["fixed_future_rt"]],
                         summary_args = list(summary_dir = summary[["fixed_future_rt"]],
                                             all_regions = FALSE),
                         logs = "rt-forecast-2/logs/fixed_future_rt",
                         delays = list(incubation_period, reporting_delay),
                         future_rt = "latest")
   }

  

# Fixed Rt ----------------------------------------------------------------

  if ("fixed_rt" %in% models) {
    std_regional_epinow(reported_cases = deaths,
                        target_folder = targets[["fixed_rt"]],
                        summary_args = list(summary_dir = summary[["fixed_rt"]],
                                            all_regions = FALSE),
                        logs = "rt-forecast-2/logs/fixed_rt",
                        delays = list(incubation_period, reporting_delay),
                       future_rt = "estimate")
  }  
  

# No delay ----------------------------------------------------------------
  
  if ("no_delay" %in% models) {
    std_regional_epinow(reported_cases = deaths,
                        target_folder = targets[["no_delay"]],
                        summary_args = list(summary_dir = summary[["no_delay"]],
                                            all_regions = FALSE),
                        logs = "rt-forecast-2/logs/no_delay",
                        future_rt = "latest")
  }
  
  
  # Add more models here ----------------------------------------------------
  return(invisible(NULL))
}

