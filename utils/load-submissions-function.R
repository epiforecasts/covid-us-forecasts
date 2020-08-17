# load submission files

# dates = "all" loads all submission files
# num_last allows you to load only the last x files

# maybe add functionality to read only specific dates later?

# 2020/08/15: adapted to any model listed in "utils/model_list.rds"
# dates = "all", "latest"
# num_last = 1: number of historical forecasts
# models = "all", "single", "ensemble", c("model_name", "model_name")

library(magrittr)

load_submission_files <- function(dates = "all", 
                                  num_last = NULL,
                                  models = "all") {
  
  # Read in model list
  model_list <- readRDS(here::here("utils", "model_list.rds"))
  
  # Set to specified models
  if(models == "all") {
    model_choice <- model_list %>%
      purrr::flatten() %>%
      names()
    
  } else {
    
    if(models == "single") {
      model_choice <- model_list$single_models %>%
        names()
      
      } else {
    
    if(models == "ensemble") {
      model_choice <- model_list$ensemble_models %>%
        names()
    }
      }
  }
  
    models <- model_list %>%
      purrr::flatten() %>%
      purrr::keep(names(.) %in% model_choice)
  
  # Get file paths for specified dates
    # For latest:
  if(dates == "latest"){
    
    files <- models %>%
      # Get file paths
      purrr::map( ~ here::here(.x[["root"]], .x[["submission_files"]],
                           list.files(path = here::here(.x[["root"]], .x[["submission_files"]]), pattern = ".csv")))
    
  } else {
    
    # For dated:
    files <- models %>%
      purrr::map( ~ here::here(.x[["root"]], .x[["submission_files"]], "dated",
                          list.files(path = here::here(.x[["root"]], .x[["submission_files"]], "dated"), 
                                      pattern = ".csv")))
    # Keep only some dates if specified
    if(!is.null(num_last)){
      files <- files %>% 
        purrr::map( ~ sort(.x, decreasing = TRUE)[1:num_last])
      }
  }
  
  # Safe read and mutate functions
  safely_read <- purrr::safely(~ read.csv(.x, colClasses = "character"))
  safely_mutate <- purrr::safely(dplyr::mutate)
  
  # Read from file paths
  data_list <- files %>%
    # Read and keep returned dfs
    purrr::map_depth(.depth = 2, ~ safely_read(.x)) %>%
    purrr::map_depth(.depth = 2, ~ purrr::pluck(.x[[1]])) 
    
  # Flatten to single df
  data_noid <- data_list %>%
    purrr::flatten_dfr() 
  
  # Set model names
  model_names <- data_list %>%
    purrr::map_depth(.depth = 2, ~ purrr::pluck(.x[[1]])) %>%
    purrr::map_depth(.depth = 2, ~ length(.x)) %>%
    unlist()
  model_names_seq <- rep(names(model_names), times = model_names)
  
  # Add model names to data
  data <- suppressMessages(dplyr::bind_cols(model_names_seq, data_noid)) %>%
    dplyr::rename("model" = "...1") %>%
    dplyr::mutate(model = stringr::str_remove_all(model, "[[:digit:]]$"))

  # Clean
  data <- data %>%
    # Add state names
    dplyr::left_join(readRDS(here::here("utils/state_codes.rds")), by = "location") %>%
    # Set variable types
    dplyr::mutate(quantile = round(as.numeric(quantile), 3),
                  value = as.numeric(value),
                  forecast_date = lubridate::ymd(forecast_date),
                  submission_date = lubridate::ymd(submission_date),
                  target_end_date = lubridate::ymd(target_end_date))

  return(data)
  
}



# Samples -----------------------------------------------------------------
# NOT CLEANED IN LINE WITH ABOVE - NOT FOR USE

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


