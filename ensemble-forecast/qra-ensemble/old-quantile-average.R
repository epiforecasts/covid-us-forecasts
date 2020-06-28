# average quantiles
# for qra go to quantile-ensembling.R

# read in csvs -----------------------------------------------------------------
# list all submissions and take the most recent one
# alternatively switch to using a date in the future?
paths <- list()


## get path of rt-submission
rt_files <- list.files(here::here("rt-forecast", "submission-files"))
rt_file <- sort(rt_files, decreasing = TRUE)[1]
paths[["rt-forecast"]] <- here::here("rt-forecast", "submission-files", rt_file)

## get path of timeseries-submissions
# deaths-on-cases
paths[["timeseries-deaths-on-cases"]] <- here::here("timeseries-forecast",
                                                    "deaths-on-cases",
                                                    "submission-files",
                                                    "latest-weekly-deaths-on-cases.csv")

# deaths only
paths[["timeseries-deaths-only"]] <- here::here("timeseries-forecast",
                                                "deaths-only",
                                                "submission-files",
                                                "latest-weekly-deaths-only.csv")

# expert elicitation


# get forecast_date
forecast_date <- stringr::str_remove(rt_file, "-rt-forecast-submission.csv")

# load in all csvs
data <- purrr::map_dfr(paths,
                       .f = data.table::fread,
                       .id = "model")


# average quantiles ------------------------------------------------------------

# store models as strings
models <- data$model %>%
  unique()
n_models <- length(models)

# locations to submit --> take from rt forecast
locations <- data %>%
  dplyr::filter(model == "rt-forecast") %>%
  .$location %>%
  unique()

# pivot into wide format
data <- data %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)

# add average
data <- data %>%
  cbind(dplyr::mutate(data, ensemble = data %>% 
                        dplyr::select(all_of(models)) %>%
                        rowMeans(na.rm = TRUE)))

# deselect all old model columns and rename ensemble to value
data <- data %>%
  dplyr::select(!any_of(models)) %>%
  dplyr::rename(value = ensemble)

# filter out locations
data <- dplyr::filter(data,
                      location %in% locations)

# store as csv submission ------------------------------------------------------
data.table::fwrite(data, here::here("final-submissions", "death-forecast",
                                    paste0(forecast_date, "-epiforecasts-ensemble1.csv")))

