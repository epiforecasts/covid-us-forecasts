# ============================================================================ #
# do ensembling with quantiles
# ============================================================================ #

library(magrittr)
library(data.table)


## again mixture of dplyr and data.table 
## see dplyr::bind_rows and do.call(dplyr::bind_rows)
## replace paste -> paste0 ideally use here to keep with all other scripts in this repo
# load deaths data -------------------------------------------------------------
death_data_inc <- load_observed_deaths(weekly = TRUE) %>%
  dplyr::rename(geography = region, 
                value = deaths) %>%
  dplyr::mutate(value_type = "inc", 
                geography = ifelse(geography == "US", "country", geography), 
                # change date to end of week 
                value_date = week + 6) %>%
  dplyr::select(-week, -period, -data_type)

death_data_cum <- load_observed_deaths(weekly = TRUE, cumulative = TRUE) %>%
  dplyr::rename(geography = region, 
                value = deaths) %>%
  dplyr::mutate(value_type = "cum", 
                geography = ifelse(geography == "US", "country", geography), 
                # change date to end of week 
                value_date = week + 6) %>%
  dplyr::select(-week, -period, -data_type)


death_data <- data.table::rbindlist(list(death_data_inc, death_data_cum))


# load previous forecasts ------------------------------------------------------
# rt files
rt_files <- list.files(here::here("rt-forecast", "submission-files"))
rt_paths <- paste("rt-forecast/submission-files/", rt_files, sep = "")

rt_forecasts <- purrr::map_dfr(rt_paths, 
                               .f = data.table::fread) %>%
  dplyr::mutate(model = "Rt forecast")

# timeseries forecasts
deaths_only_files <- list.files(here::here("timeseries-forecast", "deaths-only", "submission-files"))
deaths_only_paths <- paste("timeseries-forecast/deaths-only/submission-files/", 
                           deaths_only_files, sep = "")

deaths_only_forecasts <- purrr::map_dfr(deaths_only_paths, 
                                        .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths only forecast")


# timeseries forecasts
deaths_on_cases_files <- list.files(here::here("timeseries-forecast", "deaths-on-cases", "submission-files"))
deaths_on_cases_paths <- paste("timeseries-forecast/deaths-on-cases/submission-files/", 
                               deaths_on_cases_files, sep = "")


deaths_on_cases_forecasts <- purrr::map_dfr(deaths_on_cases_paths, 
                                            .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths on cases forecast")

# bind together 
forecasts <- data.table::rbindlist(list(rt_forecasts, deaths_only_forecasts, 
                                        deaths_on_cases_forecasts))

# format forecasts -------------------------------------------------------------

# load US location codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US")) %>%
  dplyr::rename(location = state_code)

# join forecasts with state codes
# format to get the appropriate format for qra

forecasts <- forecasts %>%
  dplyr::inner_join(state_codes) %>%
  dplyr::rename(creation_date = forecast_date, 
                value_date = target_end_date,
                geography = state_name) %>%
  dplyr::mutate(geography_scale = ifelse(geography == "US", "country", "region"), 
                geography = ifelse(geography == "US", "country", geography), 
                inc = ifelse(grepl("inc", target), "inc", "cum"), 
                value_date = as.Date(value_date), 
                creation_date = as.Date(creation_date), 
                value_type = inc) %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::select(-target, -type, -inc)


# do quantile regression and format output -------------------------------------

res <- qra::qra(forecasts, death_data, pool = c("horizon", "geography"),
                min_date = max(forecasts$creation_date) - 13)

# format results
# weekly incidences
qra_forecast <- res$ensemble %>%
  dplyr::rename(forecast_date = creation_date, 
                target_end_date = value_date) %>%
  dplyr::mutate(target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))),
                               "wk ahead",
                               value_type,
                               "death",
                               sep = " "), 
                type = "quantile") %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)


data.table::fwrite(qra_forecast, here::here("ensemble-forecast", "qra-ensemble", "submission-files",
                                    paste0(forecast_date, "-epiforecasts-ensemble1.csv")))


















# old code



# 
# 
# 
# # read in csvs ----------------------------------------------------------------- 
# # list all submissions and take the most recent one
# # alternatively switch to using a date in the future?
# paths <- list()
# 
# 
# ## get path of rt-submission
# rt_files <- list.files(here::here("rt-forecast", "submission-files"))
# rt_file <- sort(rt_files, decreasing = TRUE)[1]
# paths[["rt-forecast"]] <- here::here("rt-forecast", "submission-files", rt_file)
# 
# ## get path of timeseries-submissions
# # deaths-on-cases
# paths[["timeseries-deaths-on-cases"]] <- here::here("timeseries-forecast", 
#                                                     "deaths-on-cases",
#                                                     "submission-files",
#                                                     "latest-weekly-deaths-on-cases.csv")
# 
# # deaths only
# paths[["timeseries-deaths-only"]] <- here::here("timeseries-forecast",
#                                                 "deaths-only",
#                                                 "submission-files",
#                                                 "latest-weekly-deaths-only.csv")
# 
# # get forecast_date
# forecast_date <- stringr::str_remove(rt_file, "-rt-forecast-submission.csv")
# 
# # load in all csvs
# data <- purrr::map_dfr(paths, 
#                        .f = data.table::fread, 
#                        .id = "model")
# 
# 
# # average quantiles ------------------------------------------------------------
# 
# # store models as strings
# models <- data$model %>% 
#   unique() 
# n_models <- length(models)
# 
# # locations to submit --> take from rt forecast
# locations <- data %>%
#   dplyr::filter(model == "rt-forecast") %>%
#   .$location %>%
#   unique()
# 
# # pivot into wide format
# data <- data %>%
#   dplyr::mutate(quantile = round(quantile, digits = 2)) %>%
#   tidyr::pivot_wider(names_from = model, 
#                      values_from = value)
# 
# # get / set weights
# w <- rep(1/n_models, n_models)
# 
# # take row means (replace with weighted mean in the futurue)
# data <- as.data.table(data)[, ensemble := .(matrixStats::rowWeightedMeans(as.matrix(.SD), 
#                                                                           na.rm = TRUE, 
#                                                                           w = w)), 
#                             .SDcols = models]
# 
# # deselect all old model columns and rename ensemble to value
# data <- data %>%
#   dplyr::select(!any_of(models)) %>%
#   dplyr::rename(value = ensemble)
# 
# # filter out locations
# data <- dplyr::filter(data, 
#                       location %in% locations)
# 
# # store as csv submission ------------------------------------------------------
# data.table::fwrite(data, here::here("final-submissions", "death-forecast", 
#                                     paste0(forecast_date, "-epiforecasts-ensemble1.csv")))
# 
