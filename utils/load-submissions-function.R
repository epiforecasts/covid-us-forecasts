# load submission files

# dates = "all" loads all submission files
# num_last allows you to load only the last x files

# maybe add functionality to read only specific dates later?

library(magrittr)

load_submission_files <- function(dates = c("latest", "all"), 
                                  num_last = NULL,
                                  models = c("all",
                                            # "rt-1", 
                                             "rt-2",
                                             "deaths-only", 
                                             "deaths-on-cases", 
                                             "mean-ensemble", 
                                             "qra-ensemble",
                                             "qra-state-ensemble" #, 
                                            # "crps-ensemble"
                                            )) {
  
  
  forecasts <- list()
  
  ## Get Epinow2 Rt forecast
  if ("all" %in% models | "rt-2" %in% models) {
    if (dates[1] == "all") {
      rt2_files <- list.files(here::here("rt-forecast-2", "output", "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        rt2_files <- sort(rt2_files, decreasing = TRUE)[1:num_last]
        rt2_files <- na.exclude(rt2_files)
      }
      
      rt2_paths <- here::here("rt-forecast-2", "output", "submission-files", "dated", rt2_files)
    } else {
      rt2_paths <- here::here("rt-forecast-2", "output", "submission-files",
                             "latest-rt-2-forecast.csv")
      
    }
    
    forecasts[["rt2_forecasts"]] <- suppressMessages(purrr::map_dfr(rt2_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "Rt-Epinow2")
  }
  
  ## Get Epinow1 Rt forecast 
  # if ("all" %in% models | "rt-1" %in% models) {
  #   if (dates[1] == "all") {
  #     rt1_files <- list.files(here::here("rt-forecast", "submission-files", "dated"))
  #     
  #     if (!is.null(num_last)) {
  #       rt1_files <- sort(rt1_files, decreasing = TRUE)[1:num_last]
  #       rt2_files <- na.exclude(rt2_files)
  #     }
  #     
  #     rt1_paths <- here::here("rt-forecast", "submission-files", "dated", rt1_files)
  #   } else {
  #     rt1_paths <- here::here("rt-forecast", "submission-files",
  #                            "latest-rt-forecast-submission.csv")
  #   }
  #   forecasts[["rt1_forecasts"]] <- suppressMessages(purrr::map_dfr(rt1_paths, readr::read_csv)) %>%
  #     dplyr::mutate(model = "Rt-Epinow1")
  # }
  
  
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
  
  # Get qra-ensemble by state
  if ("qra-state-ensemble" %in% models) {
    if (dates[1] == "all") {
      
      qra_state_ensemble_files <- list.files(here::here("ensembling", "qra-state-ensemble",
                                                  "submission-files", "dated"))
      
      if (!is.null(num_last)) {
        qra_state_ensemble_files <- sort(qra_state_ensemble_files, decreasing = TRUE)[1:num_last]
      }
      
      qra_state_ensemble_paths <- here::here("ensembling", "qra-state-ensemble", 
                                       "submission-files", "dated", qra_state_ensemble_files)
      
      
    } else {
      qra_state_ensemble_paths <- here::here("ensembling", "qra-state-ensemble",
                                       "submission-files",
                                       "latest-epiforecasts-ensemble1-qra.csv") 
    }
    forecasts[["qra_state_ensemble"]] <- suppressMessages(purrr::map_dfr(qra_state_ensemble_paths, readr::read_csv)) %>%
      dplyr::mutate(model = "QRA by state")
  }
  
  ## Get crps-ensemble
  # if ("all" %in% models | "crps-ensemble" %in% models) {
  #   if (dates[1] == "all") {
  #     
  #     crps_ensemble_files <- list.files(here::here("ensembling", "crps-ensemble",
  #                                                 "submission-files", "dated"))
  #     
  #     if (!is.null(num_last)) {
  #       crps_ensemble_files <- sort(crps_ensemble_files, decreasing = TRUE)[1:num_last]
  #     }
  #     
  #     crps_ensemble_paths <- here::here("ensembling", "crps-ensemble", 
  #                                      "submission-files", "dated", crps_ensemble_files)
  #     
  #     
  #   } else {
  #     crps_ensemble_paths <- here::here("ensembling", "crps-ensemble",
  #                                      "submission-files",
  #                                      "latest-epiforecasts-ensemble1-crps.csv") 
  #   }
  #   forecasts[["crps_ensemble"]] <- suppressMessages(purrr::map_dfr(crps_ensemble_paths, readr::read_csv)) %>%
  #     dplyr::mutate(model = "CRPS ensemble")
  # }
  
  
  # Join forecasts ----------------------------------------------------------
  # and add state names
  forecasts <- dplyr::bind_rows(forecasts) %>%
    dplyr::left_join(readRDS(here::here("utils/state_codes.rds")), by = "location") %>%
    # unclear bug where there seems to be a numerical error somewhere
    dplyr::mutate(quantile = round(quantile, 3))
  
  return(forecasts)
  
}








load_sample_files <- function(dates = c("latest", "all"), 
                              num_last = NULL,
                              #horizons = c(1, 2, 3, 4, 5, 6),
                              models = c("all",
                                         "rt-1",
                                         "rt-2",
                                         "deaths-only", 
                                         "deaths-on-cases")) {
  
  ## Get Epinow2forecast samples
  if ("all" %in% models | "rt-2" %in% models) {
    
    files_rt2 <- list.files(here::here("rt-forecast-2", "output", "samples"))
    
    if (as.character(dates)[1] == "all") {
      if (!is.null(num_last)) {
        files_rt <- sort(files_rt2, decreasing = TRUE)[1:num_last]
      }
    } else if (as.character(dates)[1] == "latest") {
      files_rt2 <- sort(files_rt2, decreasing = TRUE)[1]
    } else {
      files_rt2 <- sort(files_rt2, decreasing = TRUE)
      existing_dates <- as.Date(substr(files_rt2, 1, 10))
      
      # give a one day wiggle room and create an index from the dates
      dates_to_fetch <- c(as.Date(dates), (as.Date(dates) + 1), (as.Date(dates) - 1))
      index <- 1:length(existing_dates)
      index <- index[existing_dates %in% dates_to_fetch]
      
      # if num_last is not Null, also fetch the num_last previous files
      if (!is.null(num_last)) {
        index <- index:(num_last + index)
      }
      
      files_rt2 <- files_rt2[index]
    }
    
    # name vector to get id column from map_dfr
    names(files_rt2) <- substr(files_rt2, 1, 10)
    
    rt2_forecasts <- purrr::map_dfr(.x = files_rt2, ~ readRDS(here::here("rt-forecast-2",
                                                                         "output",
                                                                         "samples", 
                                                                         .x)), 
                                   .id = "forecast_date") %>%
      dplyr::mutate(model = "Rt-2", 
                    forecast_date = as.Date(forecast_date))
  } else {
    rt2_forecasts <- NULL
  }
  
  ## Get Epinow1 forecast samples
  if ("all" %in% models | "rt-1" %in% models) {
      
    files_rt1 <- list.files(here::here("rt-forecast", "submission-samples"))
                           
    if (as.character(dates)[1] == "all") {
      if (!is.null(num_last)) {
        files_rt1 <- sort(files_rt1, decreasing = TRUE)[1:num_last]
      }
    } else if (as.character(dates)[1] == "latest") {
      files_rt1 <- sort(files_rt1, decreasing = TRUE)[1]
    } else {
      files_rt1 <- sort(files_rt1, decreasing = TRUE)
      existing_dates <- as.Date(substr(files_rt1, 1, 10))
      
      # give a one day wiggle room and create an index from the dates
      dates_to_fetch <- c(as.Date(dates), (as.Date(dates) + 1), (as.Date(dates) - 1))
      index <- 1:length(existing_dates)
      index <- index[existing_dates %in% dates_to_fetch]
      
      # if num_last is not Null, also fetch the num_last previous files
      if (!is.null(num_last)) {
        index <- index:(num_last + index)
      }
      
      files_rt1 <- files_rt1[index]
    }
    
    # name vector to get id column from map_dfr
    names(files_rt1) <- substr(files_rt1, 1, 10)
    
    rt1_forecasts <- purrr::map_dfr(.x = files_rt1, ~ readRDS(here::here("rt-forecast", 
                                                                       "submission-samples", 
                                                                       .x)), 
                                   .id = "forecast_date") %>%
      dplyr::mutate(model = "Rt", 
                    forecast_date = as.Date(forecast_date))
  } else {
    rt1_forecasts <- NULL
  }
  
  
  ## Get death only timeseries forecasts
  if ("all" %in% models | "deaths-only" %in% models) {
    files_ts_deaths <- list.files(here::here("timeseries-forecast", "deaths-only", "raw-samples", "dated"))
    
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
                                                                                 "dated",
                                                                                 .x))) %>%
      dplyr::mutate(model = "TS deaths only")
  } else {
    ts_do_forecasts <- NULL
  }
    
   
  ## Get deaths-on-cases forecasts
  if ("all" %in% models | "deaths-on-cases" %in% models) {
    
    files_ts_deaths_on_cases <- list.files(here::here("timeseries-forecast", "deaths-on-cases", "raw-samples", "dated"))
    
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
                                                                                           "dated",
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
  rt2_epiweek <- data.frame(unique(rt2_forecasts$target_end_date), 
                           lubridate::epiweek(unique(rt2_forecasts$target_end_date)))
  colnames(rt2_epiweek) <- c("target_end_date", "epiweek_target")
  
  ts_forecasts <- ts_forecasts %>%
    dplyr::mutate(forecast_date = lubridate::ymd(forecast_date)) %>%
    dplyr::left_join(rt2_epiweek, by = "epiweek_target") %>%
    dplyr::select(sample, deaths, target_end_date, model, location = state, forecast_date)
  
  
  # Join forecasts ----------------------------------------------------------
  forecasts <- dplyr::bind_rows(rt2_forecasts, rt1_forecasts, ts_forecasts)
  
  return(forecasts)
  
}






