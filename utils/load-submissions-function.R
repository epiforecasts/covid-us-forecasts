# load submission files

# dates = "all" loads all submission files
# num_last allows you to load only the last x files

# maybe add functionality to read only specific dates later?

load_submission_files <- function(dates = c("latest", "all"), 
                                  num_last = NULL,
                                  models = c("rt", 
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
    forecasts[["rt_forecasts"]] <- purrr::map_dfr(rt_paths, readr::read_csv) %>%
      dplyr::mutate(model = "Rt")
  }
  
  
  ## Get deaths-only forecasts
  if ("all" %in% models | "deaths-only" %in% models) {
    if (dates[1] == "all") {
      
      deaths_only_files <- list.files(here::here("timeseries-forecast", "deaths-only", 
                                                 "submission-files", "dated"))
      deaths_only_paths <- here::here("timeseries-forecast", "deaths-only", 
                                      "submission-files", "dated", deaths_only_files)
      
      if (!is.null(num_last)) {
        deaths_only_files <- sort(deaths_only_files, decreasing = TRUE)[1:num_last]
      }
      
      
    } else {
      deaths_only_paths <- here::here("timeseries-forecast", "deaths-only", 
                                      "submission-files",
                                      "latest-weekly-deaths-only.csv")
    }
    forecasts[["ts_deaths_only"]] <- purrr::map_dfr(deaths_only_paths, readr::read_csv) %>%
      dplyr::mutate(model = "TS deaths")
  }
  
  ## Get deaths-on-cases forecasts
  if ("all" %in% models | "deaths-on-cases" %in% models) {
    if (dates[1] == "all") {
      
      deaths_on_cases_files <- list.files(here::here("timeseries-forecast", "deaths-on-cases", 
                                                 "submission-files", "dated"))
      deaths_on_cases_paths <- here::here("timeseries-forecast", "deaths-on-cases", 
                                      "submission-files", "dated", deaths_on_cases_files)
      
      if (!is.null(num_last)) {
        deaths_on_cases_files <- sort(deaths_on_cases_files, decreasing = TRUE)[1:num_last]
      }
      
    } else {
      deaths_on_cases_paths <- here::here("timeseries-forecast", "deaths-on-cases", 
                                      "submission-files",
                                      "latest-weekly-deaths-on-cases.csv")
    }
    forecasts[["ts_deaths_on_cases"]] <- purrr::map_dfr(deaths_on_cases_paths, readr::read_csv) %>%
      dplyr::mutate(model = "TS deaths on cases")
  }
  
  
  ## Get mean average ensemble
  if ("all" %in% models | "mean-ensemble" %in% models) {
    if (dates[1] == "all") {
      
      mean_ensemble_files <- list.files(here::here("ensembling", "quantile-average",
                                                     "submission-files", "dated"))
      mean_ensemble_paths <- here::here("ensembling", "quantile-average", 
                                          "submission-files", "dated", mean_ensemble_files)
      
      if (!is.null(num_last)) {
        mean_ensemble_files <- sort(mean_ensemble_files, decreasing = TRUE)[1:num_last]
      }
      
    } else {
      mean_ensemble_paths <- here::here("ensembling", "quantile-average",
                                          "submission-files",
                                          "latest-epiforecasts-ensemble1-qa.csv") 
    }
    forecasts[["mean_ensemble"]] <- purrr::map_dfr(mean_ensemble_paths, readr::read_csv) %>%
      dplyr::mutate(model = "Mean ensemble")
  }
  
  if ("all" %in% models | "qra-ensemble" %in% models) {
    if (dates[1] == "all") {
      
      qra_ensemble_files <- list.files(here::here("ensembling", "qra-ensemble",
                                                   "submission-files", "dated"))
      qra_ensemble_paths <- here::here("ensembling", "qra-ensemble", 
                                        "submission-files", "dated", mean_ensemble_files)
      
      if (!is.null(num_last)) {
        qra_ensemble_files <- sort(qra_ensemble_files, decreasing = TRUE)[1:num_last]
      }
      
    } else {
      qra_ensemble_paths <- here::here("ensembling", "qra-ensemble",
                                        "submission-files",
                                        "latest-epiforecasts-ensemble1-qa.csv") 
    }
    forecasts[["qra_ensemble"]] <- purrr::map_dfr(mean_ensemble_paths, readr::read_csv) %>%
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
    # unclear bug
    dplyr::mutate(quantile = round(quantile, 3))
  
  return(forecasts)
  
}
