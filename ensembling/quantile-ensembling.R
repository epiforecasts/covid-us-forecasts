# 28/06/2020 - adding temporarily
# This is a direct copy of script from Nikos "restructuring" branch (ensemble-forecasts/qra-ensemble/).
# Adding here temporarily to check functionality while the PR gets sorted
# Diff: some file paths are changed to work with the current master structure: otherwise untouched


# ============================================================================ #
# do ensembling with quantiles
# ============================================================================ #

library(magrittr)
library(data.table)
library(dplyr)
library(qra)

source(here::here("utils", "get-us-data.R"))
source(here::here("ensembling", "load-observed-deaths.R"))

## again mixture of dplyr and data.table 
## see dplyr::bind_rows and do.call(dplyr::bind_rows)
## replace paste -> paste0 ideally use here to keep with all other scripts in this repo
# load US location codes
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US")) %>%
  dplyr::rename(location = state_code)

# load deaths data -------------------------------------------------------------
# death_data_inc <- load_observed_deaths(weekly = TRUE) %>%
#   dplyr::rename(geography = region, 
#                 value = deaths) %>%
#   dplyr::mutate(value_type = "inc", 
#                 geography = ifelse(geography == "US", "country", geography), 
#                 # change date to end of week 
#                 value_date = week + 6) %>%
#   dplyr::select(-week, -period, -data_type)
# 
# death_data_cum <- load_observed_deaths(weekly = TRUE, cumulative = TRUE) %>%
#   dplyr::rename(geography = region, 
#                 value = deaths) %>%
#   dplyr::mutate(value_type = "cum", 
#                 geography = ifelse(geography == "US", "country", geography), 
#                 # change date to end of week 
#                 value_date = week + 6) %>%
#   dplyr::select(-week, -period, -data_type)

###
daily_deaths_state <- get_us_deaths(data = "daily")

weekly_deaths_state <- daily_deaths_state %>%
  dplyr::group_by(state, epiweek) %>%
  dplyr::summarise(deaths = sum(deaths),
                   target_end_date = max(date)) %>%
  dplyr::filter(epiweek < max(epiweek)) %>%
  dplyr::mutate(geography_scale = "region")

weekly_deaths_national <- weekly_deaths_state %>%
  dplyr::group_by(epiweek, target_end_date) %>%
  dplyr::summarise(deaths = sum(deaths),
                   state = "US",
                   geography_scale = "country") %>%
  dplyr::ungroup()

weekly_deaths <- bind_rows(weekly_deaths_state, weekly_deaths_national) %>%
  dplyr::rename(geography = state, value = deaths) %>%
  dplyr::mutate(value_type = "inc")
  
weekly_deaths_cum <- weekly_deaths %>%
  dplyr::group_by(geography) %>%
  dplyr::mutate(value = cumsum(value),
                value_type = "cum")

###
death_data <- bind_rows(weekly_deaths, weekly_deaths_cum) %>%
  dplyr::select(-epiweek, value_date = target_end_date)

# load previous forecasts ------------------------------------------------------
# rt files
rt_files <- list.files(here::here("rt-forecast", "submission-files", "dated"))
rt_paths <- paste("rt-forecast/submission-files/dated/", rt_files, sep = "")

rt_forecasts <- purrr::map_dfr(rt_paths, 
                               .f = data.table::fread) %>%
  dplyr::mutate(model = "Rt forecast")

# timeseries forecasts
deaths_only_files <- list.files(here::here("timeseries-forecast", "deaths-only", 
                                           "submission-files", "dated"))
deaths_only_paths <- paste("timeseries-forecast/deaths-only/submission-files/dated/", 
                           deaths_only_files, sep = "")

deaths_only_forecasts <- purrr::map_dfr(deaths_only_paths, 
                                        .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths only forecast")


# timeseries forecasts
deaths_on_cases_files <- list.files(here::here("timeseries-forecast", "deaths-on-cases", 
                                               "submission-files", "dated"))
deaths_on_cases_paths <- paste("timeseries-forecast/deaths-on-cases/submission-files/dated/", 
                               deaths_on_cases_files, sep = "")


deaths_on_cases_forecasts <- purrr::map_dfr(deaths_on_cases_paths, 
                                            .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths on cases forecast")

# bind together 
forecasts <- data.table::rbindlist(list(rt_forecasts, deaths_only_forecasts, 
                                        deaths_on_cases_forecasts)) %>%
  dplyr::filter(!is.na(target_end_date))


# Identify over 100 cases in the last week
# source(here::here("utils", "states-min-last-week.R"))
# keep_states <- states_min_last_week(min_last_week = 100, last_week = 1)
# forecasts <- filter(forecasts, location %in% keep_states$state_code)


# format forecasts -------------------------------------------------------------


# join forecasts with state codes
# format to get the appropriate format for qra

forecasts_for_qra <- forecasts %>%
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

res <- qra::qra(forecasts_for_qra, death_data, pool = c("horizon", "geography"),
                min_date = max(forecasts_for_qra$creation_date) - 20, 
                enforce_normalisation = FALSE)

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

forecast_date <- qra_forecast$forecast_date %>%
  unique()

# Save --------------------------------------------------------------------

# Save in QRA folder
# Dated
data.table::fwrite(qra_forecast, here::here("ensembling", "qra-ensemble", 
                                         "submission-files", "dated", 
                                          paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# Latest
data.table::fwrite(qra_forecast, here::here("ensembling", "qra-ensemble", "submission-files",
                                         "latest-epiforecasts-ensemble1-qra.csv"))



