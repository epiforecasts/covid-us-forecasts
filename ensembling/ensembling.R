# # ============================================================================ #
# # do ensembling with predictive samples
# # ============================================================================ #
# 
# 
# # latest rt estimates to csv
# # --------------------------------
# 
# # load all rt forecasts into one data.table
# rt_forecast <- load_all_rt_forecasts(forecast_dates = "latest", 
#                                      forecast_adjustment = 16, 
#                                      weekly = FALSE)
# 
# # save rt-forecasts
# data.table::fwrite(rt_forecast, here::here("ensembling", "latest_forecasts", "latest_rt_forecast.csv"))
# 
# toy <- rt_forecast %>%
#   dplyr::mutate(deaths = deaths + 1, 
#                 model = "toymodel")
# 
# data.table::fwrite(toy, here::here("ensembling", "latest_forecasts", "latest_toy_forecast.csv"))
# 
# # latest timeseries estimates to csv
# # --------------------------------
# 
# # missing
# 
# # code can be either here - then we can also drop the csv files completely
# # or in the timeseries folder and here we just load in csv files
# 
# 
# 
# 
# # load all predictions from csv
# # --------------------------------
# 
# files <- list.files(here::here("ensembling", "latest_forecasts"))
# 
# 
# # combine data and rename columns
# df <- purrr::map_dfr(files, 
#                      function(file) {
#                        data.table::fread(here::here("ensembling", "latest_forecasts", file))
#                      }) %>%
#   dplyr::rename(y_pred = deaths, 
#                 geography = region,
#                 sample_nr = sample) %>%
#   # data issue
#   dplyr::filter(geography %in% c("US", "Alabama"))
# 
# ## some data issue: not all regions have the full number of samples
# # filter_out <- df %>% 
# #   dplyr::group_by(geography) %>%
# #   dplyr::summarise(n = dplyr::n())
# 
# # read in weights
# weights <- data.table::fread(here::here("ensembling", "stacking_weights.csv"))
# w <- weights$stacking_weights
# # w <- rep(1/2, length(w))
# 
# mix <- stackr::mixture_from_samples(df, w)
# 
# # write ensemble to csv for submission
# 
# 
# ## Test Zone
# 
# 
# forecast_date <- as.Date(unique(df$forecast_date))
# 
# int_forecast_date <- dplyr::case_when(weekdays(forecast_date) == "Sunday" ~ forecast_date,
#                                       TRUE ~ as.Date(lubridate::floor_date(forecast_date, unit = "week", week_start = 7)))
# 
# state_codes <- tigris::fips_codes %>%
#   dplyr::select(state_code, state_name) %>%
#   unique() %>%
#   rbind(c("US", "US"))
# 
# 
# inc <- mix %>% 
#   dplyr::rename(sample = sample_nr, 
#                 cases = y_pred) %>%
#   dplyr::mutate(sample = as.integer(sample), 
#                 date = as.Date(date)) %>%
#   # # filter by samples temporary fix until this is addressed in EpiNow/estimate_R0.R
#   # dplyr::group_by(sample, date) %>%
#   # dplyr::slice(1) %>%
#   # dplyr::ungroup() %>%
#   dplyr::mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7)) %>% 
#   dplyr::filter(date >= int_forecast_date) %>%
#   dplyr::group_by(sample, week) %>%
#   dplyr::summarise(week_cases = sum(cases, na.rm = TRUE)) %>%
#   dplyr::ungroup()
# 
# cum <- inc %>%
#   dplyr::group_by(sample) %>%
#   dplyr::mutate(week_cases = cumsum(week_cases) + cumulative_deaths_data)
# 
# 
# # =============================================================================
# # take the function from format_submission.R and take it outside the function
# # to reuse here. 
# process_data = function(df, name){
#   
#   df <- df %>%
#     dplyr::group_by(week) %>%
#     dplyr::group_modify( ~ {
#       quantile(.x$week_cases, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
#         tibble::enframe(name = "quantile", value = "value") %>%
#         dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
#     }) %>%
#     dplyr::mutate(forecast_date = forecast_date,
#                   target_end_date = week + 6,
#                   location = state_codes$state_code[state_codes$state_name == loc_name],
#                   type = "quantile",
#                   horizon = ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))),
#                   target = paste(ceiling(as.numeric(difftime(target_end_date, forecast_date, unit = "week"))),
#                                  "wk ahead",
#                                  name,
#                                  "death",
#                                  sep = " ")) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(horizon <= 4) %>%
#     dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)
#   
#   df <- df %>%
#     dplyr::bind_rows(df %>%
#                        dplyr::filter(quantile == 0.5) %>%
#                        dplyr::mutate(type = "point") %>%
#                        dplyr::select(-quantile)) %>%
#     dplyr::mutate(value = floor(value))
#   
#   return(df)
#   
# }
# # =============================================================================
# 
# out <- process_data(inc, "inc") %>%
#   dplyr::bind_rows(process_data(cum, "cum"))



# ============================================================================ #
# do ensembling with quantiles
# ============================================================================ #

# read in csvs ----------------------------------------------------------------- 
# list all submissions and take the most recent one
# alternatively switch to using a date in the future?

paths <- list()

# get path of rt-submission
rt_files <- list.files(here::here("rt-forecast", "submission-files"))
rt_file <- sort(files, decreasing = TRUE)[1]
paths[["rt-forecast"]] <- here::here("rt-forecast", "submission-files", rt_file)

# get path of timeseries-submission
timeseries_files <- list.files(here::here("timeseries-forecast", "submission-files"))
timeseries_file <- sort(files, decreasing = TRUE)[1]
paths[["timeseries-forecast"]] <- here::here("timeseries-forecast", "submission-files", rt_file)

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

# pivot into wide format
data <- data %>%
  tidyr::pivot_wider(names_from = model, 
                     values_from = value)


# get / set weights
# change to something more elaborate in the future
w <- rep(1/n_models, n_models)

# take row means (replace with weighted mean in the futurue)
data <- as.data.table(data)[, ensemble := .(value = rowMeans(.SD)), .SDcols = models]

# deselect all old model columns and rename ensemble to value
data <- data %>%
  dplyr::select(!any_of(models)) %>%
  dplyr::rename(value = ensemble)


# store as csv submission ------------------------------------------------------
data.table::fwrite(data, here::here("final-submissions", "death-forecast", 
                                    paste0(forecast_date, "-epiforecasts-ensemble1.csv")))
