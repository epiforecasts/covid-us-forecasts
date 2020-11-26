
require(lubridate, quietly = TRUE)
# Set up running a single Rt forecast -------------------------------------

run_rt_forecast <- function(deaths, 
                            submission_date, 
                            models = list(),
                            rerun = FALSE) {
  
  if (missing(submission_date)) {
    rerun <- TRUE
  }
  # Set up directories for models -------------------------------------------
  if (length(models) == 0) {
    source("utils/meta-model-list.R")
    models <- names(model_list$single_models)[grepl("rt", names(model_list$single_models))] 
    models <- gsub("rt2_", "", models)
    }
  
  targets <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/state"))
  names(targets) <- models
  
  summary <- purrr::map(models, ~ paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/summary"))
  names(summary) <- models
  
  
  if (!rerun) {
    targets_present <- purrr::map_lgl(targets, 
              ~ dir.exists(file.path(., "US", lubridate::ymd(submission_date) - lubridate::days(1))))
    
    models <- models[!targets_present]
  }

  # Format for epinow2 ------------------------------------------------------
  
  # Deaths data
  if (!missing(submission_date)) {
    deaths <- dplyr::filter(deaths, date < submission_date)
  }
  deaths <- setDT(deaths)
  deaths <- deaths[, .SD[date >= (max(date) - lubridate::weeks(8))], by = region]
  data.table::setorder(deaths, date)
  
  # Rt global options
  rt <- opts_list(rt_opts(), deaths) %>%
    purrr::transpose()
  
  # - Population data
  rt$pop <- suppressMessages(readr::read_csv("utils/state_pop_totals.csv")) %>%
    dplyr::pull(tot_pop, name = state_name) %>%
    as.list()
  
  # - Prior
  rt$prior <- purrr::map(rt$prior, ~ list(mean = 1, sd = 0.2))
  
  
  # Set up common settings --------------------------------------------------

  std_regional_epinow <- purrr::partial(regional_epinow, 
                                        horizon = 30, 
                                        generation_time = generation_time,
                                        non_zero_points = 14,
                                        stan = stan_opts(samples = 2000,
                                                         warmup = 250, 
                                                         cores = no_cores, 
                                                         chains = ifelse(no_cores <= 4, 4, no_cores)),
                                        output = c("region", "samples", "summary", "timing"),
                                        obs = obs_opts(scale = list(mean = 0.005, sd = 0.001)))
  # Run Rt - ORIGINAL -------------------------------------------------------
   if ("original" %in% models) {
     # Rt options
     rt_original <- rt
     rt_original$future <- purrr::map(rt_original$future, ~ "project")
     rt_original <- purrr::transpose(rt_original)
     
     # Run model
     std_regional_epinow(reported_cases = deaths,
                         target_folder = targets[["original"]],
                         summary_args = list(summary_dir = summary[["original"]],
                                             all_regions = FALSE),
                         logs = "rt-forecast-2/logs/original",
                         delays = delay_opts(incubation_period, reporting_delay),
                         rt = rt_original)
   }

  # Run Rt - FIXED RT --------------------------------------------------

   if ("fixed_future_rt" %in% models) {
     # Rt options
     rt_fixed_future <- rt
     rt_fixed_future$future <- purrr::map(rt_fixed_future$future, ~ "latest")
     rt_fixed_future <- purrr::transpose(rt_fixed_future)
     
     # Run model
     std_regional_epinow(reported_cases = deaths,
                         target_folder = targets[["fixed_future_rt"]],
                         summary_args = list(summary_dir = summary[["fixed_future_rt"]],
                                             all_regions = FALSE),
                         logs = "rt-forecast-2/logs/fixed_future_rt",
                         delays = delay_opts(incubation_period, reporting_delay),
                         rt = rt_fixed_future)
   }

  

# Fixed Rt ----------------------------------------------------------------

  if ("fixed_rt" %in% models) {
    # Rt options
    rt_fixed <- rt
    rt_fixed$future <- purrr::map(rt_fixed$future, ~ "estimate")
    rt_fixed <- purrr::transpose(rt_fixed)
    
    # Run model
    std_regional_epinow(reported_cases = deaths,
                        target_folder = targets[["fixed_rt"]],
                        summary_args = list(summary_dir = summary[["fixed_rt"]],
                                            all_regions = FALSE),
                        logs = "rt-forecast-2/logs/fixed_rt",
                        delays = delay_opts(incubation_period, reporting_delay),
                        rt = rt_fixed)
  }  
  

# No delay ----------------------------------------------------------------
  
  if ("no_delay" %in% models) {
    # Run model
    std_regional_epinow(reported_cases = deaths,
                        target_folder = targets[["no_delay"]],
                        summary_args = list(summary_dir = summary[["no_delay"]],
                                            all_regions = FALSE),
                        logs = "rt-forecast-2/logs/no_delay",
                         rt = purrr::transpose(rt))
  }
  
  # Backcalculation ----------------------------------------------------------------
  
  if ("backcalc" %in% models) {
    # Run model
    std_regional_epinow(reported_cases = deaths,
                        target_folder = targets[["backcalc"]],
                        summary_args = list(summary_dir = summary[["backcalc"]],
                                            all_regions = FALSE),
                        rt = NULL,
                        delays = delay_opts(incubation_period, reporting_delay),
                        logs = "rt-forecast-2/logs/backcalculation")
  }
  
  
  # Add more models here ----------------------------------------------------
  return(invisible(NULL))
}

