library(magrittr)
library(data.table)
library(purrr)


# This script switches between data.table and tidyverse in a not very clean way
# it would be less confusing if it stuck to one (there is very little data.table so perhaps
# these should be recoded.)
## Script doesn't run as functions used aren't called
##  Use here not paste. 
# ============================================================================ #
# load all submissions
# ============================================================================ #

# rt submissions ---------------------------------------------------------------
rt_files <- list.files(here::here("rt-forecast", "submission-files"))
rt_paths <- paste0("rt-forecast/submission-files/", rt_files)

rt_forecasts <- purrr::map_dfr(rt_paths, 
                               .f = data.table::fread) %>%
  dplyr::mutate(model = "Rt forecast")

# deaths-only forecasts --------------------------------------------------------
deaths_only_files <- list.files(here::here("timeseries-forecast", "deaths-only", "submission-files"))
deaths_only_paths <- paste0("timeseries-forecast/deaths-only/submission-files/", 
                           deaths_only_files)

deaths_only_forecasts <- purrr::map_dfr(deaths_only_paths, 
                                        .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths only forecast")


# deaths-on-cases forecasts ----------------------------------------------------
deaths_on_cases_files <- list.files(here::here("timeseries-forecast", "deaths-on-cases", "submission-files"))
deaths_on_cases_paths <- paste0("timeseries-forecast/deaths-on-cases/submission-files/", 
                               deaths_on_cases_files)

deaths_on_cases_forecasts <- purrr::map_dfr(deaths_on_cases_paths, 
                                            .f = data.table::fread) %>%
  dplyr::mutate(model = "Deaths on cases forecast")


# qra-ensemble forecasts ----------------------------------------------------
qra_files <- list.files(here::here("ensemble-forecast", "qra-ensemble", "submission-files"))
qra_paths <- paste0("ensemble-forecast/qra-ensemble/submission-files/", 
                   deaths_on_cases_files)

qra_forecasts <- purrr::map_dfr(deaths_on_cases_paths, 
                                            .f = data.table::fread) %>%
  dplyr::mutate(model = "qra ensemble")



# bind together
forecasts <- data.table::rbindlist(list(rt_forecasts, deaths_only_forecasts, 
                                        deaths_on_cases_forecasts, 
                                        qra_forecasts)) %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date), 
                quantile = round(quantile, digits = 3))


# ============================================================================ #
# load death data
# ============================================================================ #

source(here::here("utils", "load-observed-deaths.R"))

# load incidence data ----------------------------------------------------------
death_data_inc <- load_observed_deaths(weekly = TRUE) %>%
  dplyr::rename(geography = region, 
                obs_deaths =  deaths) %>%
  dplyr::mutate(value_type = "inc", 
                geography = ifelse(geography == "US", "country", geography), 
                # change date to end of week 
                target_end_date = week + 6) %>%
  dplyr::select(-week, -period, -data_type)

# load cumulative data ---------------------------------------------------------
death_data_cum <- load_observed_deaths(weekly = TRUE, cumulative = TRUE) %>%
  dplyr::rename(geography = region, 
                obs_deaths =  deaths) %>%
  dplyr::mutate(value_type = "cum", 
                geography = ifelse(geography == "US", "country", geography), 
                # change date to end of week 
                target_end_date = week + 6) %>%
  dplyr::select(-week, -period, -data_type)

# bind together ----------------------------------------------------------------
death_data <- data.table::rbindlist(list(death_data_inc, death_data_cum), 
                                    use.names = TRUE) %>%
  dplyr::rename(location = state_code) 


# ============================================================================ #
# combine deaths and forecasts and reformat
# ============================================================================ #

# bind together ----------------------------------------------------------------
combined <- forecasts %>%
  dplyr::inner_join(death_data)

# format -----------------------------------------------------------------------
# only score on incidences? --> discuss

combined <- combined %>%
  dplyr::filter(type == "quantile", 
                value_type == "inc") %>%
  dplyr::select(-value_type, -type) %>%
  dplyr::mutate(range = abs(100 - 2 * 100 * as.numeric(quantile)), 
                boundary = ifelse(quantile < 0.5, "lower", "upper"))

# add lower bound for median a second time
combined <- combined %>%
  rbind(combined %>%
          dplyr::filter(quantile == 0.5) %>%
          dplyr::mutate(boundary = "lower"))



# pivot wider
data <- combined %>%
  tidyr::pivot_wider(values_from = value, names_from = c(boundary), 
                     id_cols = c(forecast_date, target, target_end_date, 
                                 location, model, range, geography, obs_deaths, epiweek)) %>%
  as.data.table()


# score and summarise
data[, `:=`("Interval_Score", scoringutils::interval_score(obs_deaths, 
                                                           lower, upper, range))]

summarised_scores <- data %>%
  dplyr::group_by(forecast_date, target, geography, model) %>%
  dplyr::summarise(interval_score = mean(Interval_Score, na.rm = TRUE)) 


## plotting
# currently not working as intended I think
library(ggplot2)

plotdf <- summarised_scores %>%
  dplyr::filter(grepl("inc", target)) 

plotdf %>%
  ggplot(aes(x = interval_score, y = model, 
             color = model)) +
  geom_linerange(aes(xmin = quantile(interval_score, 0.25), 
                 xmax = quantile(interval_score, 0.75))) +
  geom_point(aes(x = median(interval_score))) + 
  # facet_wrap(~target) + 
  cowplot::theme_cowplot() +
  coord_cartesian(xlim=c(0, quantile(plotdf$interval_score, 0.8))) + 
  theme(text = element_text(family = "Sans Serif"))


eval_date <- Sys.Date()

if(!dir.exists(here::here("evaluation-plots", eval_date))) {
  dir.create(here::here("evaluation-plots", eval_date))
}






















# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ============================================================================ #
# # load all models vs. true values
# # ============================================================================ #
# 
# # files <- list.files(here::here("evaluation", "observed_vs_forecast"))
# # 
# # # load in (toy) data
# # files <- files[-1]
# # 
# # # combine data
# # df <- purrr::map_dfr(files, 
# #                      function(file) {
# #                        data.table::fread(here::here("evaluation", "observed_vs_forecast", file))
# #                      })
# 
# 
# # ============================================================================ #
# # evaluate forecasts
# # ============================================================================ #
# 
# # do automatic scoring with scoringutils
# scored_forecasts <- df %>%
#   dplyr::group_by(horizon, region) %>%
#   tidyr::nest() %>%
#   dplyr::mutate(scores = purrr::map(data, 
#                                     function(x) {
#                                       scoringutils::eval_forecasts(x, summarised = TRUE)
#                                       })) %>%
#   tidyr::unnest(cols = c(data)) %>%
#   dplyr::group_by(forecast_date, id, region) %>%
#   dplyr::summarise(scores = unique(scores)) %>%
#   tidyr::unnest(cols = c(scores)) %>%
#   dplyr::mutate(horizon = as.numeric(as.Date(id) - as.Date(forecast_date)))
# 
# # also add median and iqr and ci
# # this is done to use pre-existing plotting functions
# missing_scores <- df %>%
#   dplyr::group_by(forecast_date, id, region, model) %>%
#   dplyr::summarise(median = median(predictions), 
#                    iqr = IQR(predictions), 
#                    ci = scoringutils::interval_score(true_values = predictions, 
#                                                      lower = quantile(predictions, prop = 0.025), 
#                                                      upper = quantile(predictions, prop = 0.975), 
#                                                      95))
# 
# # join together to obtain all scores and forecasts
# full <- dplyr::inner_join(scored_forecasts, missing_scores) %>%
#   dplyr::rename(dss = DSS, 
#                 crps = CRPS, 
#                 calibration = pit_p_val) %>%
#   dplyr::mutate(logs = NA)
# 
# # summarise scores
# summarised_scores <- EpiSoon::summarise_scores(full)
# 
# 
# 
# # ============================================================================ #
# # plot evaluation
# # ============================================================================ #
# 
# # use pre-existing functions
# 
# 
