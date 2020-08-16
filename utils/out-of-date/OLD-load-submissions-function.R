# 2020/08/15
# load-submissions-function.R
# Preserving for the record - for Nikos to keep or delete as needed


load_submission_files <- function(dates = c("latest", "all"), 
                                  num_last = NULL,
                                  models = c("all")) {
  
## Get Epinow2 Rt forecast
if ("all" %in% models | "rt-2" %in% models) {
  if (dates[1] == "all") {
    rt2_files <- list.files(here::here("rt-forecast-2", "output", "original", "submission-files", "dated"))
    
    if (!is.null(num_last)) {
      rt2_files <- sort(rt2_files, decreasing = TRUE)[1:num_last]
      rt2_files <- na.exclude(rt2_files)
    }
    
    rt2_paths <- here::here("rt-forecast-2", "output", "submission-files", "dated", rt2_files)
  } else {
    rt2_paths <- here::here("rt-forecast-2", "output", "submission-files",
                            "latest.csv")
    
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
