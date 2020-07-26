# ============================================================================ #
# do ensembling with predictive samples
# ============================================================================ #

library(magrittr)
library(dplyr)

# load all rt forecasts into one data.table ------------------------------------
files_rt <- list.files(here::here("rt-forecast", "submission-samples"))

rt_forecasts <- purrr::map_dfr(.x = files_rt, ~ readRDS(here::here("rt-forecast", 
                                                                "submission-samples", 
                                                                .x)))


# make fake data ---------------------------------------------------------------
# fake_forecasts <- rt_forecasts %>%
#   dplyr::mutate(deaths = deaths + rpois(1, 1), 
#                 model = "fake model")


# get timeseries forecasts ------------------------------------------------

# deaths only
files_ts_deaths <- list.files(here::here("timeseries-forecast", "deaths-only", "raw-samples", "dated"))

ts_do_forecasts <- purrr::map_dfr(.x = files_ts_deaths, ~ readRDS(here::here("timeseries-forecast",
                                                                   "deaths-only",
                                                                   "raw-samples", 
                                                                   "dated",
                                                                   .x)))
# epiweek to target date
source(here::here("utils", "dates-to-epiweek.R"))

epiweek_to_target <- unique(ts_do_forecasts$epiweek_target)
rt_epiweek <- data.frame(unique(rt_forecasts$target_end_date), 
                         lubridate::epiweek(unique(rt_forecasts$target_end_date)))
colnames(rt_epiweek) <- c("target_end_date", "epiweek_target")



ts_do_forecasts <- ts_do_forecasts %>%
  mutate(forecast_date = lubridate::ymd(forecast_date),
         model = "TS deaths only") %>%
  left_join(rt_epiweek, by = "epiweek_target") %>%
  select(sample, deaths, target_end_date, model, location = state, forecast_date)

# deaths on cases
files_ts_deaths_on_cases <- list.files(here::here("timeseries-forecast", "deaths-on-cases", "raw-samples"))

ts_doc_forecasts <- purrr::map_dfr(.x = files_ts_deaths_on_cases, ~ readRDS(here::here("timeseries-forecast",
                                                                   "deaths-on-cases",
                                                                   "raw-samples", 
                                                                   .x)))

ts_doc_forecasts <- ts_doc_forecasts %>%
  mutate(forecast_date = lubridate::ymd(forecast_date),
         model = "TS deaths on cases") %>%
  left_join(rt_epiweek, by = "epiweek_target") %>%
  select(sample, deaths, target_end_date, model, location = state, forecast_date)

# join and create full set -----------------------------------------------------
full_set <- dplyr::bind_rows(rt_forecasts, ts_do_forecasts, ts_doc_forecasts) %>%
  dplyr::group_by(forecast_date, target_end_date, location, sample) %>%
  dplyr::add_tally() %>% # switched to tally (does the same thing) as add_count beats my memory limit!
  dplyr::ungroup() %>%
  dplyr::filter(n == max(n)) %>%
  dplyr::select(-n) %>%
  dplyr::rename(y_pred = deaths)


# load deaths ------------------------------------------------------------------
source(here::here("utils", "get-us-data.R"))
deaths <- get_us_deaths(data = "daily") %>%
  dplyr::group_by(epiweek, state) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  dplyr::rename(location = state, 
                y_obs = deaths)

# code to get from epiweek to target date copied from Kath
epiweek_to_date <- tibble::tibble(date = seq.Date(from = (as.Date("2020-01-01")), 
                                                  by = 1, length.out = 365)) %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date),
                day = weekdays(date)) %>%
  dplyr::filter(day == "Saturday") %>%
  dplyr::select(target_end_date = date, epiweek)

# join deaths with past forecasts and reformat
combined <- full_set %>%
  dplyr::inner_join(epiweek_to_date, by = "target_end_date") %>%
  dplyr::inner_join(deaths, by = c("location", "epiweek")) 

# current problem: stackr isn't able to deal with different forecast horizons

# use only horizon = 1 ---------------------------------------------------------

combined_filtered <- combined %>%
  dplyr::mutate(horizon = round(as.numeric(target_end_date - forecast_date) / 7)) %>%
  dplyr::filter(horizon == 1) %>%
  dplyr::select(date = target_end_date, 
                sample_nr = sample, 
                model, 
                geography = location, 
                y_obs, 
                y_pred)

w <- stackr::crps_weights(data = combined_filtered)

# make ensemble based on the weights -------------------------------------------

# load in latest data (maybe make a dedicated function for that)

files <- list.files(here::here("rt-forecast", "submission-samples"))

file <- sort(files, decreasing = TRUE)[1]

forecast_date <- as.Date(substr(file, 1, 10))

current_forecast <- purrr::map_dfr(.x = file, ~ readRDS(here::here("rt-forecast", 
                                                                    "submission-samples", 
                                                                    .x))) %>%
  dplyr::rename(y_pred = deaths, 
                date = target_end_date)


# add fake forecast
current_forecast <- current_forecast %>%
  dplyr::bind_rows(current_forecast %>%
                     dplyr::mutate(model = "fake model")) %>%
  dplyr::rename(sample_nr = sample, 
                geography = location) %>%
  dplyr::filter(!is.na(as.integer(y_pred))) %>%
  dplyr::mutate(y_pred = as.integer(y_pred))


# make ensemble ----------------------------------------------------------------
ensemble <- stackr::mixture_from_samples(current_forecast, weights = w)



  
# make submission --------------------------------------------------------------

state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US"))

incidences <- ensemble %>%
  dplyr::rename(state_name = geography) %>%
  dplyr::inner_join(state_codes, by = "state_name") %>%
  dplyr::rename(location = state_code, 
                target_end_date = date, 
                value = y_pred) %>%
  dplyr::mutate(forecast_date = forecast_date, 
                horizon = round(as.numeric(target_end_date - forecast_date) / 7), 
                inc_or_cum = "inc")


cumulative <- incidences %>%
  dplyr::group_by(location, target_end_date, sample_nr) %>%
  dplyr::summarise(value = sum(value), 
                   horizon = unique(horizon), .groups = "drop") %>%
  dplyr::mutate(inc_or_cum = "cum")

combined <- incidences %>%
  dplyr::bind_rows(cumulative) %>%
  dplyr::group_by(target_end_date, location, inc_or_cum, horizon) %>%
  dplyr::group_modify( ~ {
    quantile(.x$value, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
      tibble::enframe(name = "quantile", value = "value") %>%
      dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
  })  %>%
  dplyr::ungroup() %>%
  dplyr::mutate(target = paste(horizon,
                               "wk ahead",
                               inc_or_cum,
                               "death",
                               sep = " "), 
                type = "quantile") %>%
  dplyr::bind_rows(combined %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point",
                                   quantile = NA)) %>%
  dplyr::mutate(forecast_date = forecast_date) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)



# write dated file
data.table::fwrite(combined, here::here("ensembling", "qra-ensemble", 
                                            "submission-files","dated",
                                            paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# write Latest files
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-qra.csv")))


