# Format Rt into quantiles with incident and cumulative ready for submission
# Packages ----------------------------------------------------------------

require(here)
require(dplyr)
require(lubridate)


# Function to format Rt forecast for one region ------------------------------------------------------

## Arguments
# loc: path to folder: e.g. loc <- here::here("rt-forecast", "state")
# loc_name: name of region: e.g. loc_name <- "Alabama"
# forecast_date: date forecast is made e.g. forecast_date <- Sys.Date()
# forecast_adjustment: numeric e.g. 4

format_rt_forecast <- function(loc = NULL, loc_name = NULL,
                               forecast_date = NULL,
                               submission_date = NULL,
                               forecast_adjustment = 0,
                               horizon_weeks = 5,
                               state_data_cumulative = NULL,
                               state_data_daily = NULL,
                               version = "1.0", 
                               samples = FALSE,
                               replace_missing_with_latest = FALSE){

  print(loc_name)

# Get case count --------------------------------------------------

  # Cumulative
  cumulative_data <- state_data_cumulative
  
  # cumulative_deaths_state <- cumulative_data %>%
  #   dplyr::group_by(epiweek) %>%
  #   dplyr::filter(date == max(date)) %>%
  #   dplyr::ungroup()
  
  cumulative_deaths_state <- cumulative_data %>%
    dates_to_epiweek() %>%
    dplyr::filter(epiweek_end == TRUE)

  cumulative_deaths_national <- cumulative_deaths_state %>%
    dplyr::group_by(epiweek) %>%
    dplyr::summarise(deaths = sum(deaths),
                     state = "US", .groups = "drop_last")

  cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)

  last_week_cumulative_deaths <- cumulative_deaths %>%
    dplyr::filter(epiweek == lubridate::epiweek(forecast_date)-1,
                  state == loc_name) %>%
    .$deaths
  

  # Weekly incident
  weekly_data <- state_data_daily %>%
    dates_to_epiweek() %>%
    dplyr::filter(epiweek_full == TRUE)
  
  weekly_deaths_state <- weekly_data %>%
    dplyr::group_by(state, epiweek) %>%
    dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
    dplyr::ungroup()
  
  weekly_deaths_national <- weekly_data %>%
    dplyr::group_by(epiweek) %>%
    dplyr::summarise(deaths = sum(deaths),
                     state = "US", .groups = "drop_last")
  
  weekly_deaths <- dplyr::bind_rows(weekly_deaths_state, weekly_deaths_national)
  
  # Previous epiweek count per state (loc)
  last_week_incident_deaths <- weekly_deaths %>%
    dplyr::filter(epiweek == lubridate::epiweek(forecast_date)-1,
                  state == loc_name) %>%
    .$deaths

  

# Get estimates ---------------------------------------------------------------

  file_loc <- paste0(loc, "/", forecast_date)
  
  
  # Optional: if forecast date doesn't exist: default to latest
  if(replace_missing_with_latest){
    if(!dir.exists(file_loc)) {
      file_loc <- paste0(loc, "/latest")
    print(paste0(loc_name, ": forecast date doesn't exist, defaulting to latest"))
   }
  }
  
  # If no Rt estimate made, return NULL
    if (!dir.exists(file_loc) ){
      print(paste0(loc_name, ": no Rt files"))
      return(NULL)
    } 
    

    nowcast <- file.path(file_loc,"nowcast.rds")
    nowcast <- readRDS(nowcast) %>%
      dplyr::select(date, sample, cases, type) %>%
      dplyr::filter(type %in% "infection_upscaled") %>%
      dplyr::select(-type) %>%
      dplyr::mutate(sample = as.integer(sample))

    forecast <- file.path(file_loc, "time_varying_params.rds")

    if (file.exists(forecast)) {
      forecast <- readRDS(forecast)
    } else {
      warning("Folder exists, but file time_varying_params.rds not found")
      return(NULL)
    }
    
        
    
    ## Check to see if a forecast is present; if not return NULL
    if (is.null(forecast$raw_case_forecast)) {
      print(paste0(loc_name, ": no forecast"))
      return(NULL)
    } 
      
 

# Set up data and dates ---------------------------------------------------


      forecast <- forecast$raw_case_forecast
      
      submission_date_epiweek <- lubridate::epiweek(submission_date)

      

# Incident weekly results -------------------------------------------------

      incident_forecast <- forecast %>%
        dplyr::filter(rt_type == "forecast") %>%
        dplyr::mutate(sample = as.integer(sample)) %>%
        # filter by samples temporary fix until this is addressed in EpiNow/estimate_R0.R
        dplyr::group_by(sample, date) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        #
        dplyr::bind_rows(nowcast) %>%
        # Only give results for complete epiweeks
        dates_to_epiweek() %>%
        dplyr::filter(epiweek >= submission_date_epiweek & epiweek_full == TRUE) %>%
        dplyr::group_by(sample, epiweek) %>%
        dplyr::summarise(epiweek_cases = sum(cases, na.rm = TRUE),
                         .groups = "drop_last") %>%
        dplyr::ungroup() %>%
        epiweek_to_date()
      
      
      if (samples) {
        
        incident_forecast_samples <- incident_forecast %>%
          dplyr::rename(target_end_date = epiweek_end_date, 
                        deaths = epiweek_cases) %>%
          dplyr::mutate(model = "EpiSoon Rt", 
                        location = loc_name, 
                        forecast_date = forecast_date) %>%
          dplyr::select(sample, deaths, target_end_date, model, location)
          
        
        return(incident_forecast_samples)
      }

# Cumulative results ------------------------------------------------------

      cumulative_forecast <- incident_forecast %>%
        dplyr::group_by(sample) %>%
        dplyr::mutate(epiweek_cases = cumsum(epiweek_cases)) %>%
        dplyr::ungroup()
      
     cumulative_forecast$epiweek_cases <- cumulative_forecast$epiweek_cases + last_week_cumulative_deaths[1]
      

# Process forecasts into quantiles ---------------------------------------------------

        process_data = function(df, name){
          
        df <- df %>%
          dplyr::group_by(epiweek) %>%
          dplyr::group_modify( ~ {
            quantile(.x$epiweek_cases, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
              tibble::enframe(name = "quantile", value = "value") %>%
              dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
          }) %>%
          epiweek_to_date() %>%
          dplyr::mutate(forecast_date = forecast_date,
                        submission_date = submission_date,
                        submission_date_epiweek = submission_date_epiweek,
                        target_epiweek = epiweek,
                        target_end_date = epiweek_end_date,
                        location = loc_name,
                        type = "quantile",
                        horizon = target_epiweek - submission_date_epiweek + 1,
                        target = paste(horizon,
                                       "wk ahead",
                                       name,
                                       "death",
                                       sep = " ")) %>%
          dplyr::ungroup() %>%
          dplyr::filter(horizon <= horizon_weeks) %>%
          dplyr::select(forecast_date, submission_date, target, target_end_date, 
                        location, type, quantile, value)
        
        df <- df %>%
          dplyr::bind_rows(df %>%
                             dplyr::filter(quantile == 0.5) %>%
                             dplyr::mutate(type = "point") %>%
                             dplyr::select(-quantile)) %>%
          dplyr::mutate(value = floor(value))
        
        return(df)
        
      }
      
# Bind formatted estimates and return
      incident_formatted <- process_data(incident_forecast, "inc") 
      cumulative_formatted <- process_data(cumulative_forecast, "cum")
      
      out <- dplyr::bind_rows(incident_formatted, cumulative_formatted)
      
      return(out)
    }