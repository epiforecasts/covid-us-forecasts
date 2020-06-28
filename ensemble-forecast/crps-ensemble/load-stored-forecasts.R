#' @title Load in Raw Case Forecasts from Rt model
#' 
#' @details 
#' Loads raw case forecasts made by the Rt model for a single date. 
#' Function is adapted from EpiNow::get_timeseries()
#' 
#' @param results_dir directory to the results
#' @param date date for which to get the raw case forecasts
#' 
#' @return data.frame with raw case forecasts
#'
#' @export
#' @examples
#' 

## Move these to ensembling

get_raw_case_forecasts <- function (results_dir = NULL, date = NULL) {
  if (is.null(date)) {
    date <- "latest"
  }
  # raw_case_forecast is in position five
  rt_index <- 5
  regions <- list.files(results_dir)
  names(regions) <- regions
  load_data <- purrr::safely(EpiNow::load_nowcast_result)
  incidence <- purrr::map_dfr(regions, ~load_data("time_varying_params.rds", 
                                                  ., result_dir = results_dir, date = date)[[1]][[rt_index]], 
                              .id = "region")
  return(incidence)
}




#' @title Load in Complete Set of Raw Case Forecasts from Rt model
#' 
#' @details 
#' loads in national and subnational case forecasts from the Rt model and 
#' formats them
#' 
#' @param forecast_dates specify the forecast dates for which to load corresponding
#' forecasts. If \code{forecast_dates = NULL} all dates for which there are
#' US forecasts available will be loaded
#' @param forecast_adjustment shift dates for the forecasts by a certain amount
#' @param weekly return weekly instead of daily data
#' 
#' @return data.frame with all Rt model forecasts
#'
#' @export
#' @examples
#'

load_all_rt_forecasts <- function(forecast_dates = NULL, 
                                  forecast_adjustment = 16, 
                                  weekly = FALSE) {
  
  avail_dates <- list.files(here::here("rt-forecast", "rt-forecast", "national", "US"))
  avail_dates <- avail_dates[avail_dates != "latest"]
  names(avail_dates) <- avail_dates
  
  if (is.null(forecast_dates)) {
    # if no dates where given use the dates for which we have US level forecasts
    # maybe change later to something more sensible, like dates where he have 
    # US as well as subnational forecasts
    forecast_dates <- avail_dates
  } else if (forecast_dates[1] == "latest") {
    avail_dates <- sort(avail_dates, decreasing = TRUE)
    forecast_dates <- avail_dates[1]
  }
  
  ## get national forecast
  dir <- here::here("rt-forecast", "rt-forecast", "national")
  deaths_national <- purrr::map_dfr(forecast_dates, ~ get_raw_case_forecasts(results_dir = dir, 
                                                                    date = .), 
                                    .id = "forecast_date") %>%
    # filter by samples temporary fix until this is addressed in EpiNow/estimate_R0.R
    dplyr::filter(rt_type == "forecast") %>%
    dplyr::mutate(sample = as.integer(sample)) %>%
    dplyr::group_by(sample, date) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  
  ## get subnational forecasts
  dir <- here::here("rt-forecast", "rt-forecast", "state")
  deaths_subnational <- purrr::map_dfr(forecast_dates, ~ get_raw_case_forecasts(results_dir = dir, 
                                                                          date = .), 
                                    .id = "forecast_date") %>%
    # filter by samples temporary fix until this is addressed in EpiNow/estimate_R0.R
    dplyr::filter(rt_type == "forecast") %>%
    dplyr::mutate(sample = as.integer(sample)) %>%
    dplyr::group_by(sample, date) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  
  # summarise cases to weekly cases
  if (weekly) {
    deaths_national <- deaths_national %>%
      dplyr::mutate(date = date + lubridate::days(forecast_adjustment),
                    week = lubridate::floor_date(date, unit = "week", week_start = 7), 
                    horizon = as.integer(horizon / 7 + 1)) %>%
      dplyr::group_by(forecast_date, region, sample, week, horizon) %>%
      dplyr::summarise(weekly_cases = sum(cases)) 
      
    deaths_subnational <- deaths_subnational %>%
      dplyr::mutate(date = date + lubridate::days(forecast_adjustment),
                    week = lubridate::floor_date(date, unit = "week", week_start = 7), 
                    horizon = as.integer(horizon / 7 + 1)) %>%
      dplyr::group_by(forecast_date, region, sample, week, horizon) %>%
      dplyr::summarise(weekly_cases = sum(cases)) 
  }
  
  # bind together and make some formatting
  forecasts <- data.table::rbindlist(list(deaths_national, deaths_subnational))
  suppressWarnings(forecasts[, c("rt_type", "type") := NULL]) # can be done better
  data.table::setnames(forecasts, "cases", "deaths")
  forecasts[, model := "EpiSoon_rt"]
  
  if (!weekly) {
    # shift dates by 11 + 5 = 16
    forecasts[, date := date + lubridate::days(forecast_adjustment)]
    
    # add week to make output consistent
    forecasts[, week := lubridate::floor_date(date, unit = "week", week_start = 7)]
  }
  
  return(forecasts)
}



#' @title Load Observed Deaths Together With Past Forecasts
#' 
#' @details 
#' Loads death data from file and the forecasts made by the different models
#' 
#' @param forecast_dates forecast_dates specify the forecast dates for which to load corresponding
#' forecasts. If \code{forecast_dates = NULL} all dates for which there are
#' US forecasts available will be loaded
#' 
#' @return data.frame with obsered deaths
#'
#' @export
#' @examples
#' df <- get_true_and_forecast()
#' 
#' data.table::fwrite(df, here::here("evaluation", "observed_vs_forecast", "rt-forecast-vs-obs.csv"))





get_true_and_forecast <- function(forecast_dates = NULL) {
  
  # add functionality for the dates here
  # 
  #
  
  rt_forecast <- load_all_rt_forecasts() %>%
    dplyr::rename(predictions = deaths, 
                  id = date)
  
  forecasts <- data.table::rbindlist(list(rt_forecast))
  
  true_values <- load_observed_deaths() %>%
    dplyr::rename(id = date, 
                  true_values = deaths)
  
  combined <- dplyr::inner_join(forecasts, true_values)
  
  return(combined)
}



