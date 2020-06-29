# average quantiles
# for qra go to quantile-ensembling.R

# read in csvs -----------------------------------------------------------------
# list all submissions and take the most recent one
# alternatively switch to using a date in the future?
paths <- list()


## get path of rt-submission
paths[["rt-forecast"]] <- here::here("rt-forecast",
                                      "submission-files",
                                      "latest-rt-forecast-submission.csv")

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


# load in all csvs
data <- purrr::map_dfr(paths,
                       .f = data.table::fread,
                       .id = "model")

# get forecast_date
forecast_date <- unique(dplyr::pull(data, forecast_date))


# average quantiles ------------------------------------------------------------

# store models as strings
models <- data$model %>%
  unique()
n_models <- length(models)


# pivot into wide format
data <- data %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)

# add average; rename ensemble to value
data <- data %>%
 dplyr::mutate(ensemble = data %>% 
                  dplyr::select(all_of(models)) %>%
                  rowMeans(na.rm = TRUE)) %>%
  dplyr::rename(value = ensemble)

# # filter out locations
# # locations to submit = 100+ deaths last week
# source(here::here("utils", "states-min-last-week.R"))
# keep_states <- states_min_last_week(min_last_week = 100, last_week = 1)
# 
# data <- dplyr::filter(data,
#                       location %in% keep_states$state_code)

# Deselect model specific columns
data <- dplyr::select(data, -dplyr::any_of(models))


# store as csv submission ------------------------------------------------------
# Store in QA folder
# Dated
data.table::fwrite(data, here::here("ensembling", "quantile-average", 
                                    "submission-files","dated",
                                    paste0(forecast_date, "-epiforecasts-ensemble1-qa.csv")))
# Latest
data.table::fwrite(data, here::here("ensembling", "quantile-average", "submission-files",
                                    paste0("latest-epiforecasts-ensemble1-qa.csv")))
