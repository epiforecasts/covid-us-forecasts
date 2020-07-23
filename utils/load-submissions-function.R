# load submission files

# dates = "all" loads all submission files
# num_last allows you to load only the last x files

# maybe add functionality to read only specific dates later?

library(magrittr)

load_submission_files <- function(dates = c("latest", "all"), 
                                  num_last = NULL,
                                  models = c("all",
                                             "rt", 
                                             "deaths-only", 
                                             "deaths-on-cases", 
                                             "mean-ensemble", 
                                             "qra-ensemble")) {
  
  
  forecasts <- list()
  
  ## Get most recent Rt forecast 
  if ("all" %in% models | "rt" %in% models) {
    if (dates[1] == "all") {
      rt_files <- list.files(here::here("rt-forecast", "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        rt_files <- sort(rt_files, decreasing = TRUE)[1:num_last]
      }
      
      rt_paths <- here::here("rt-forecast", "submission-files", "dated", rt_files)
    } else {
      rt_paths <- here::here("rt-forecast", "submission-files",
                             "latest-rt-forecast-submission.csv")
    }
    forecasts[["rt_forecasts"]] <- suppressMessages(purrr::map_dfr(rt_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "Rt")
  }
  
  
  ## Get deaths-only forecasts
  if ("all" %in% models | "deaths-only" %in% models) {
    if (dates[1] == "all") {
      
      deaths_only_files <- list.files(here::here("timeseries-forecast", "deaths-only", 
                                                 "submission-files", "dated"))
      if (!is.null(num_last)) {
        deaths_only_files <- sort(deaths_only_files, decreasing = TRUE)[1:num_last]
      }
      
      deaths_only_paths <- here::here("timeseries-forecast", "deaths-only", 
                                      "submission-files", "dated", deaths_only_files)
      
      
    } else {
      deaths_only_paths <- here::here("timeseries-forecast", "deaths-only", 
                                      "submission-files",
                                      "latest-weekly-deaths-only.csv")
    }
    forecasts[["ts_deaths_only"]] <- suppressMessages(purrr::map_dfr(deaths_only_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "TS deaths")
  }
  
  ## Get deaths-on-cases forecasts
  if ("all" %in% models | "deaths-on-cases" %in% models) {
    if (dates[1] == "all") {
      
      deaths_on_cases_files <- list.files(here::here("timeseries-forecast", "deaths-on-cases", 
                                                 "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        deaths_on_cases_files <- sort(deaths_on_cases_files, decreasing = TRUE)[1:num_last]
      }
      
      deaths_on_cases_paths <- here::here("timeseries-forecast", "deaths-on-cases", 
                                      "submission-files", "dated", deaths_on_cases_files)
      
    } else {
      deaths_on_cases_paths <- here::here("timeseries-forecast", "deaths-on-cases", 
                                      "submission-files",
                                      "latest-weekly-deaths-on-cases.csv")
    }
    forecasts[["ts_deaths_on_cases"]] <- suppressMessages(purrr::map_dfr(deaths_on_cases_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "TS deaths on cases")
  }
  
  
  ## Get mean average ensemble
  if ("all" %in% models | "mean-ensemble" %in% models) {
    if (dates[1] == "all") {
      
      mean_ensemble_files <- list.files(here::here("ensembling", "quantile-average",
                                                     "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        mean_ensemble_files <- sort(mean_ensemble_files, decreasing = TRUE)[1:num_last]
      }
      
      mean_ensemble_paths <- here::here("ensembling", "quantile-average", 
                                          "submission-files", "dated", mean_ensemble_files)
      
    } else {
      mean_ensemble_paths <- here::here("ensembling", "quantile-average",
                                          "submission-files",
                                          "latest-epiforecasts-ensemble1-qa.csv") 
    }
    forecasts[["mean_ensemble"]] <- suppressMessages(purrr::map_dfr(mean_ensemble_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "Mean ensemble")
  }
  
  # Get qra-ensemble
  if ("all" %in% models | "qra-ensemble" %in% models) {
    if (dates[1] == "all") {
      
      qra_ensemble_files <- list.files(here::here("ensembling", "qra-ensemble",
                                                   "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        qra_ensemble_files <- sort(qra_ensemble_files, decreasing = TRUE)[1:num_last]
      }
      
      qra_ensemble_paths <- here::here("ensembling", "qra-ensemble", 
                                        "submission-files", "dated", qra_ensemble_files)
      
      
    } else {
      qra_ensemble_paths <- here::here("ensembling", "qra-ensemble",
                                        "submission-files",
                                        "latest-epiforecasts-ensemble1-qra.csv") 
    }
    forecasts[["qra_ensemble"]] <- suppressMessages(purrr::map_dfr(qra_ensemble_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "QRA ensemble")
  }
  
  # Join forecasts ----------------------------------------------------------
  # and add state names
  forecasts <- dplyr::bind_rows(forecasts) %>%
    dplyr::left_join(tigris::fips_codes %>%
                       dplyr::select(state_code, state = state_name) %>%
                       unique() %>%
                       rbind(c("US", "US")),
                     by = c("location" = "state_code")) %>%
    # unclear bug where there seems to be a numerical error somewhere
    dplyr::mutate(quantile = round(quantile, 3))
  
  return(forecasts)
  
}








load_sample_files <- function(dates = c("latest", "all"), 
                              num_last = NULL,
                              #horizons = c(1, 2, 3, 4, 5, 6),
                              models = c("all",
                                         "rt", 
                                         "deaths-only", 
                                         "deaths-on-cases")) {
  
  ## Get most recent Rt forecast samples
  if ("all" %in% models | "rt" %in% models) {
      
    files_rt <- list.files(here::here("rt-forecast", "submission-samples"))
                           
    if (as.character(dates)[1] == "all") {
      if (!is.null(num_last)) {
        files_rt <- sort(files_rt, decreasing = TRUE)[1:num_last]
      }
    } else if (as.character(dates)[1] == "latest") {
      files_rt <- sort(files_rt, decreasing = TRUE)[1]
    } else {
      files_rt <- sort(files_rt, decreasing = TRUE)
      existing_dates <- as.Date(substr(files_rt, 1, 10))
      
      # give a one day wiggle room and create an index from the dates
      dates_to_fetch <- c(as.Date(dates), (as.Date(dates) + 1), (as.Date(dates) - 1))
      index <- 1:length(existing_dates)
      index <- index[existing_dates %in% dates_to_fetch]
      
      # if num_last is not Null, also fetch the num_last previous files
      if (!is.null(num_last)) {
        index <- index:(num_last + index)
      }
      
      files_rt <- files_rt[index]
    }
    
    # name vector to get id column from map_dfr
    names(files_rt) <- substr(files_rt, 1, 10)
    
    rt_forecasts <- purrr::map_dfr(.x = files_rt, ~ readRDS(here::here("rt-forecast", 
                                                                       "submission-samples", 
                                                                       .x)), 
                                   .id = "forecast_date") %>%
      dplyr::mutate(model = "Rt", 
                    forecast_date = as.Date(forecast_date))
  } else {
    rt_foreasts <- NULL
  }
  
  
  ## Get death only timeseries forecasts
  if ("all" %in% models | "deaths-only" %in% models) {
    files_ts_deaths <- list.files(here::here("timeseries-forecast", "deaths-only", "raw-samples"))
    
    if (as.character(dates)[1] == "all") {
      if (!is.null(num_last)) {
        files_ts_deaths <- sort(files_ts_deaths, decreasing = TRUE)[1:num_last]
      }
    } else if (as.character(dates)[1] == "latest") {
      files_ts_deaths <- sort(files_ts_deaths, decreasing = TRUE)[1]
    } else {
      files_ts_deaths <- sort(files_ts_deaths, decreasing = TRUE)
      existing_dates <- as.Date(substr(files_ts_deaths, 1, 10))
      
      # give a one day wiggle room and create an index from the dates
      dates_to_fetch <- c(as.Date(dates), (as.Date(dates) + 1), (as.Date(dates) - 1))
      index <- 1:length(existing_dates)
      index <- index[existing_dates %in% dates_to_fetch]
      
      # if num_last is not Null, also fetch the num_last previous files
      if (!is.null(num_last)) {
        index <- index:(num_last + index)
      }
      
      files_ts_deaths <- files_ts_deaths[index]
    }
    
    ts_do_forecasts <- purrr::map_dfr(.x = files_ts_deaths, ~ readRDS(here::here("timeseries-forecast",
                                                                                 "deaths-only",
                                                                                 "raw-samples", 
                                                                                 .x))) %>%
      dplyr::mutate(model = "TS deaths only")
  } else {
    ts_do_forecasts <- NULL
  }
    
   
  ## Get deaths-on-cases forecasts
  if ("all" %in% models | "deaths-on-cases" %in% models) {
    
    files_ts_deaths_on_cases <- list.files(here::here("timeseries-forecast", "deaths-on-cases", "raw-samples"))
    
    if (as.character(dates)[1] == "all") {
      if (!is.null(num_last)) {
        files_ts_deaths_on_cases <- sort(files_ts_deaths_on_cases, decreasing = TRUE)[1:num_last]
      }
    } else if (as.character(dates)[1] == "latest") {
      files_ts_deaths_on_cases <- sort(files_ts_deaths_on_cases, decreasing = TRUE)[1]
    } else {
      files_ts_deaths_on_cases <- sort(files_ts_deaths_on_cases, decreasing = TRUE)
      existing_dates <- as.Date(substr(files_ts_deaths_on_cases, 1, 10))
      
      # give a one day wiggle room and create an index from the dates
      dates_to_fetch <- c(as.Date(dates), (as.Date(dates) + 1), (as.Date(dates) - 1))
      index <- 1:length(existing_dates)
      index <- index[existing_dates %in% dates_to_fetch]
      
      # if num_last is not Null, also fetch the num_last previous files
      if (!is.null(num_last)) {
        index <- index:(num_last + index)
      }
      
      files_ts_deaths_on_cases <- files_ts_deaths_on_cases[index]
    }
    
    ts_doc_forecasts <- purrr::map_dfr(.x = files_ts_deaths_on_cases, ~ readRDS(here::here("timeseries-forecast",
                                                                                           "deaths-on-cases",
                                                                                           "raw-samples", 
                                                                                           .x))) %>%
      dplyr::mutate(model = "TS deaths on cases")
  } else {
    ts_doc_forecasts <- NULL
  }
  
  ## join timeseries forecasts together and convert epiweek to target_end_date
  
  ts_forecasts <- dplyr::bind_rows(ts_do_forecasts, ts_doc_forecasts)
  
  # convert epiweek to target_end_date
  # take the existing target_end_dates from the rt forecasts and add epiweek_target
  # this code is a bit confusing - maybe should switch to a version that doesn't 
  # rely on the rt-forecasts? 
  
  epiweek_to_target <- unique(ts_do_forecasts$epiweek_target)
  rt_epiweek <- data.frame(unique(rt_forecasts$target_end_date), 
                           lubridate::epiweek(unique(rt_forecasts$target_end_date)))
  colnames(rt_epiweek) <- c("target_end_date", "epiweek_target")
  
  ts_forecasts <- ts_forecasts %>%
    dplyr::mutate(forecast_date = lubridate::ymd(forecast_date)) %>%
    dplyr::left_join(rt_epiweek, by = "epiweek_target") %>%
    dplyr::select(sample, deaths, target_end_date, model, location = state, forecast_date)
  
  
  
  
  
  # Join forecasts ----------------------------------------------------------
  forecasts <- dplyr::bind_rows(rt_forecasts, ts_forecasts)
  
  return(forecasts)
  
}






