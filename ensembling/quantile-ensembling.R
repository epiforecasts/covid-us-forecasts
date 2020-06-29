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
source(here::here("utils", "get-us-data.R"))
source(here::here("ensembling", "load-observed-deaths.R"))

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


death_data <- data.table::rbindlist(list(death_data_inc, death_data_cum), 
                                    use.names = TRUE)

# remove epiweek column --> ensembling is based on the target_date
death_data <- death_data %>%
  dplyr::select(-epiweek) %>%
  dplyr::rename(location = state_code)

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

forecast_date <- qra_forecast$forecast_date %>%
  unique()

data.table::fwrite(forecasts, here::here("ensembling", "qra-ensemble", "submission-files",
                                            paste0(forecast_date, "-epiforecasts-ensemble1.csv")))

