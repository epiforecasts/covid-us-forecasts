
# Packages ----------------------------------------------------------------

require(here)
require(dplyr)
require(lubridate)


# Function to format Rt forecast for one region ------------------------------------------------------

## Arguments
# loc: path to folder: e.g. loc <- here::here("rt-forecast", "state")
# loc_name: name of region: e.g. loc_name <- "Alabama"
# last_week_deaths: threshold for only getting estimates from states with minimum cumulative deaths in the last week
# forecast_date: date forecast is made e.g. forecast_date <- Sys.Date()
# forecast_adjustment: numeric e.g. 4

format_rt_forecast <- function(loc = NULL, loc_name = NULL,
                                forecast_date = NULL,
                                forecast_adjustment = 0,
                                version = "1.0"){

  print(loc_name)

# Get case count --------------------------------------------------

  # Cumulative
  cumulative_data <- readRDS(here::here("data", "deaths-data-cumulative.rds")) %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date))
  
  cumulative_deaths_state <- cumulative_data %>%
    dplyr::group_by(epiweek) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  cumulative_deaths_national <- cumulative_data %>%
    dplyr::group_by(epiweek) %>%
    dplyr::filter(date == max(date)) %>%
    suppressMessages(dplyr::summarise(deaths = sum(deaths)))

  cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)



  # Weekly
  weekly_data <- readRDS(here::here("data", "deaths-data-daily.rds")) %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date))
  
  weekly_deaths_state <- weekly_data %>%
    dplyr::group_by(state, epiweek) %>%
    suppressMessages(dplyr::summarise(deaths = sum(deaths))) %>%
    dplyr::ungroup()
  
  weekly_deaths_national <- weekly_data %>%
    dplyr::group_by(epiweek) %>%
    suppressMessages(dplyr::summarise(deaths = sum(deaths)))
  
  weekly_deaths <- dplyr::bind_rows(weekly_deaths_state, weekly_deaths_national)
  
    # Previous epiweek  count per state (loc)
  last_week_incident_deaths <- weekly_deaths %>%
    dplyr::filter(epiweek == lubridate::epiweek(forecast_date-1),
                  state == loc_name) %>%
    .$deaths

  

# Get estimates ---------------------------------------------------------------

  
  ## Check if latest folder exists; if not, return NULL
  if (!dir.exists(paste0(loc, "/latest/")) ){
    return(NULL)
    
  } 

    nowcast <- file.path(loc,"/", "latest/nowcast.rds")
    nowcast <- readRDS(nowcast) %>%
      dplyr::select(date, sample, cases, type) %>%
      dplyr::filter(type %in% "infection_upscaled") %>%
      dplyr::select(-type) %>%
      dplyr::mutate(sample = as.integer(sample))

    forecast <- file.path(loc, "/", "latest/time_varying_params.rds")
    forecast <- readRDS(forecast)
    
    ## Check to see if a forecast is present; if not return NULL
    if (is.null(forecast$raw_case_forecast)) {
      
      return(NULL)
      
    } 
      


# Set up data and dates ---------------------------------------------------


      forecast <- forecast$raw_case_forecast
      
      forecast_date_epiweek <- lubridate::epiweek(forecast_date)

      

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
        dplyr::mutate(date = date + lubridate::days(forecast_adjustment),
                      epiweek = lubridate::epiweek(date)) %>% 
        dplyr::filter(epiweek >= forecast_date_epiweek) %>%
        dplyr::group_by(sample, epiweek) %>%
        suppressMessages(dplyr::summarise(epiweek_cases = sum(cases, na.rm = TRUE),
                         epiweek_end_date = max(date))) %>%
        dplyr::ungroup()

# Cumulative results ------------------------------------------------------

      cumulative_forecast <- incident_forecast %>%
        dplyr::group_by(sample) %>%
        dplyr::mutate(epiweek_cases = cumsum(epiweek_cases)) %>%
        dplyr::ungroup()
      
      cumulative_forecast$epiweek_cases <- cumulative_forecast$epiweek_cases + last_week_cumulative_deaths
      

# Process forecasts into quantiles ---------------------------------------------------

        process_data = function(df, name){
          
          epiweek_to_date <- df %>%
            dplyr::select(epiweek_end_date, epiweek) %>%
            dplyr::distinct()
          
          
        df <- df %>%
          dplyr::group_by(epiweek) %>%
          dplyr::group_modify( ~ {
            quantile(.x$epiweek_cases, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
              tibble::enframe(name = "quantile", value = "value") %>%
              dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
          }) %>%
          dplyr::left_join(epiweek_to_date, by = "epiweek") %>%
          dplyr::mutate(forecast_date = forecast_date,
                        forecast_date_epiweek = forecast_date_epiweek,
                        target_epiweek = epiweek,
                        target_end_date = epiweek_end_date,
                        location = loc_name,
                        type = "quantile",
                        target = paste((target_epiweek - forecast_date_epiweek) + 1,
                                       "wk ahead",
                                       name,
                                       "death",
                                       sep = " ")) %>%
          dplyr::ungroup() %>%
          #dplyr::filter(horizon <= 4) %>%
          dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)
        
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
    


# Format all Rt regional estimates ----------------------------------------


# Packages ----------------------------------------------------------------

require(here)
require(stringr)
require(dplyr)
require(purrr)

# Control parameters ------------------------------------------------------

forecast_date <- Sys.Date()
forecast_dir <- here::here("rt-forecast")  # Assumes forecasts are in national and regional subfolders


# Update US data saved in /data ----------------------------------------------------------

source(here::here("utils", "get-us-data.R"))

deaths_data_cumulative <- get_us_deaths(data = "cumulative")
saveRDS(deaths_data_cumulative, file = here::here("data", "deaths-data-cumulative.rds"))

deaths_data_daily <- get_us_deaths(data = "daily")
saveRDS(deaths_data_daily, file = here::here("data", "deaths-data-daily.rds"))


# Find forecasts ----------------------------------------------------------

forecasts <- c(list.dirs(file.path(forecast_dir, "state"), recursive = FALSE),
               list.dirs(file.path(forecast_dir, "national"), recursive = FALSE))

names(forecasts) <- forecasts %>%
  stringr::str_remove(file.path(forecast_dir, "state/")) %>%
  stringr::str_remove(file.path(forecast_dir, "national/"))

# Extract forecasts -------------------------------------------------------

region_forecasts <- purrr::map2_dfr(.x = forecasts, .y = names(forecasts),
                                    ~ format_rt_forecast(loc = .x, loc_name = .y,
                                                         forecast_date = forecast_date,
                                                         forecast_adjustment = 11 + 5))

# Add state codes

state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US"))

region_forecasts <- region_forecasts %>%
  dplyr::mutate(state_name = gsub("/", "", x = location)) %>%
  dplyr::left_join(state_codes, by = "state_name") %>%
  dplyr::mutate(location = state_code) %>%
  dplyr::select(-state_name, -state_code)

# Save forecast -----------------------------------------------------------

readr::write_csv(region_forecasts,
                 paste0("rt-forecast/submission-files/", forecast_date, "-rt-forecast-submission.csv"))



