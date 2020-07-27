# ============================================================================ #
# do ensembling with predictive samples
# ============================================================================ #

library(magrittr)
library(dplyr)

if(!exists("forecast_date")) {
  forecast_date <- Sys.Date()
}

# ---------------------------------------------------------------------------- #
# ------------------ get weights based on past observations ------------------ #
# ---------------------------------------------------------------------------- #

# load past forecasts ----------------------------------------------------------
# filter out current forecast in order to optimise only on previous forecasts
# also filter only horizon == 1 for stackr optimisation
today <- forecast_date

past_forecasts <- load_sample_files(dates = forecast_date, num_last = 2) %>%
  dplyr::mutate(horizon = round(as.numeric(target_end_date - forecast_date) / 7)) %>%
  dplyr::filter(horizon == 1, 
                forecast_date < today - 1)

# epiweek to target date
source(here::here("utils", "dates-to-epiweek.R"))

epiweek_to_target <- unique(ts_do_forecasts$epiweek_target)

rt_epiweek <- data.frame(unique(rt_forecasts$target_end_date), 
                         lubridate::epiweek(unique(rt_forecasts$target_end_date)))
colnames(rt_epiweek) <- c("target_end_date", "epiweek_target")


# join and create full set -----------------------------------------------------
# should maybe switch that to data.table in the future

full_set <- past_forecasts %>%
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

# join deaths with past forecasts and reformat and select appropriate columns
combined <- full_set %>%
  dplyr::inner_join(epiweek_to_date, by = "target_end_date") %>%
  dplyr::inner_join(deaths, by = c("location", "epiweek")) %>%
  dplyr::select(date = target_end_date, 
                sample_nr = sample, 
                model, 
                geography = location, 
                y_obs, 
                y_pred)

w <- stackr::crps_weights(data = combined)


# ---------------------------------------------------------------------------- #
# ------------------ create ensemble and make submission  -------------------- #
# ---------------------------------------------------------------------------- #


# load in latest data ----------------------------------------------------------
current_forecast <- load_sample_files(dates = forecast_date) %>%
  dplyr::rename(geography = location, 
                date = target_end_date, 
                sample_nr = sample, 
                y_pred = deaths) %>%
  # some values are apparently too large to fit into an integer. 
  # These should be filtered out I think
  dplyr::mutate(y_pred = as.integer(y_pred)) %>%
  dplyr::filter(!is.na(y_pred))


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
                type = "quantile")

combined <- combined %>%
  dplyr::bind_rows(combined %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point",
                                   quantile = NA)) %>%
  dplyr::mutate(forecast_date = forecast_date) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)



# write dated file
data.table::fwrite(combined, here::here("ensembling", "crps-ensemble", 
                                            "submission-files","dated",
                                            paste0(forecast_date, "-epiforecasts-ensemble1-crps.csv")))
# write Latest files
data.table::fwrite(combined, here::here("ensembling", "crps-ensemble", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-crps.csv")))


