
require(lubridate, quietly = TRUE)
# Set up running a single Rt forecast -------------------------------------

run_rt_forecast <- function(deaths, submission_date, rerun = FALSE) {
  
  if (missing(submission_date)) {
    rerun <- TRUE
  }
  # Set up directories for models -------------------------------------------
  models <- list("original", "fixed_future_rt", "no_daily_effect", "fixed_rt")
  
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
  deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(12))], by = region]
  
  data.table::setorder(deaths, date)
  
  
  
  # Set up common settings --------------------------------------------------
  
  std_regional_epinow <- purrr::partial(regional_epinow, 
                                        generation_time = generation_time,
                                        horizon = 30,
                                        samples = 2000,
                                        warmup = 500,
                                        burn_in = 14,
                                        non_zero_points = 14,
                                        adapt_delta = 0.98,
                                        cores = no_cores,
                                        chains = ifelse(no_cores <= 2, 2, no_cores),
                                        return_estimates = FALSE, verbose = FALSE
  )
  # Run Rt - ORIGINAL -------------------------------------------------------
  # if ("original" %in% models) {
  #   std_regional_epinow(reported_cases = deaths,
  #                       target_folder = targets[["original"]],
  #                       summary_dir = summary[["original"]],
  #                       delays = list(incubation_period, reporting_delay))
  # }

  # Run Rt - FIXED RT --------------------------------------------------
  
  # if ("fixed_future_rt" %in% models) {
  #   std_regional_epinow(reported_cases = deaths,
  #                       target_folder = targets[["fixed_future_rt"]],
  #                       summary_dir = summary[["fixed_future_rt"]],
  #                       delays = list(incubation_period, reporting_delay),
  #                       fixed_future_rt = TRUE)
  # }

  

# Run no weekly reporting -------------------------------------------------
# 
#   if ("no_daily_effect" %in% models) {
#     std_regional_epinow(reported_cases = deaths,
#                         target_folder = targets[["no_daily_effect"]],
#                         summary_dir = summary[["no_daily_effect"]],
#                         delays = list(incubation_period, reporting_delay),
#                         fixed_future_rt = TRUE,
#                         estimate_week_eff = FALSE)
#   }  
#   
  

# Minimal delay -----------------------------------------------------------

  # if ("minimal_delay" %in% models) {
  #   std_regional_epinow(reported_cases = deaths,
  #                       target_folder = targets[["minimal_delay"]],
  #                       summary_dir = summary[["minimal_delay"]],
  #                       delays = list(minimal_delay),
  #                       fixed_future_rt = TRUE)
  # }  
  # 

# Fixed Rt ----------------------------------------------------------------

  if ("fixed_rt" %in% models) {
    std_regional_epinow(reported_cases = deaths[, 
                      breakpoint := data.table::fifelse(date == (max(date) - lubridate::days(28)), 1, 0)],
                        target_folder = targets[["fixed_rt"]],
                        summary_dir = summary[["fixed_rt"]],
                        delays = list(incubation_period, reporting_delay),
                        fixed = TRUE,
                        estimate_breakpoints = TRUE)
  }  
  
  
  
  # Add more models here ----------------------------------------------------
  
  return(invisible(NULL))
}

