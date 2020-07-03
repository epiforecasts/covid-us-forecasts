# # Score and compare daily timeseries models
# # 
# source(here::here("utils", "get-us-data.R"))
# source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-deaths-only-daily.R"))
# source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-deaths-on-cases-daily.R"))
# source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-log-lin-daily.R"))
# source(here::here("timeseries-forecast", "daily", "forecast-functions", "ts-log-log-daily.R"))
# 
# ### STILL IN WEEKS - TBC
# 
# # Get data ----------------------------------------------------------------
# 
# deaths_state <- get_us_deaths(data = "daily")
# deaths_national <- deaths_state %>%
#   group_by(date) %>%
#   summarise(deaths = sum(deaths),
#             epiweek = max(epiweek)) %>%
#   mutate(state = "US")
# 
# cases_state <- get_us_cases(data = "daily")
# cases_national <- cases_state %>%
#   group_by(date) %>%
#   summarise(cases = sum(cases),
#             epiweek = max(epiweek)) %>%
#   mutate(state = "US")
# 
# 
# 
# # Split into observed and future datasets ---------------------------------
# 
# week_lag <- 4
#   
#  # Cases
#   train_case_data <- cases_national %>% 
#     dplyr::filter(epiweek <= (max(epiweek) - week_lag))
#   
#   # Deaths 
#   # Forecast model input should be daily
#   train_death_data <- deaths_national %>% 
#     dplyr::filter(epiweek <= (max(epiweek) - week_lag))
#   
#   # Test data should be in epiweeks as this is what the forecast outputs
#   test_data <- deaths_national %>% 
#     dplyr::anti_join(train_death_data, by = "epiweek") %>% 
#     dplyr::mutate(sample = 1) %>% 
#     dplyr::group_by(epiweek) %>%
#     dplyr::summarise(deaths = sum(deaths)) %>%
#     dplyr::mutate(h = 1:dplyr::n()) %>% 
#     dplyr::filter(h <= week_lag) %>%
#     ungroup()
#   
#   # Forecast
#   sample_count <- 1000
#   horizon_weeks <- week_lag
#   right_truncate_weeks <- 1
#   format <- TRUE
#   quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#   case_quantile <- 0.5
#   
#   
# 
# # Untransformed - raw cases and death counts ------------------------------------------------------------------
# 
#  # Standard (currently used) function
#   scoring_forecast <- ts_deaths_on_cases_forecast(case_data = train_case_data,
#                                              deaths_data = train_death_data,
#                                              case_quantile = case_quantile,
#                                              sample_count = sample_count, 
#                                              horizon_weeks = horizon_weeks,
#                                              right_truncate_weeks = right_truncate_weeks,
#                                              format = FALSE)
#   
#   test_date <- unique(test_data$epiweek)
#   scoring_date <- unique(scoring_forecast$epiweek)
#   
#   # Set up format for scoring
#   samples_linear <- scoring_forecast %>% 
#     dplyr::ungroup() %>%
#     dplyr::mutate(epiweek = recode(epiweek, !!! setNames(test_date, scoring_date))) %>%
#     dplyr::select(epiweek, value, sample) %>%
#     dplyr::inner_join(test_data, by = "epiweek") %>%
#     dplyr::select(id = epiweek, 
#                   true_values = deaths, predictions = value, sample) %>%
#     dplyr::mutate(model = "linear")
#  
# # Log cases / raw deaths -----------------------------------------------------------------
# 
#   # Partly log adapted function
#   scoring_forecast_loglin <- ts_loglin_forecast(case_data = train_case_data,
#                                                  deaths_data = train_death_data,
#                                                  case_quantile = case_quantile,
#                                                  sample_count = sample_count, 
#                                                  horizon_weeks = horizon_weeks,
#                                                  right_truncate_weeks = right_truncate_weeks,
#                                                  format = FALSE)
#  
#  # Set up format for scoring
#  samples_loglin <- scoring_forecast_loglin %>% 
#    dplyr::ungroup() %>%
#    dplyr::mutate(epiweek = recode(epiweek, !!! setNames(test_date, scoring_date))) %>%
#    dplyr::select(epiweek, value, sample) %>%
#    dplyr::inner_join(test_data, by = "epiweek") %>%
#    dplyr::select(id = epiweek, 
#                  true_values = deaths, predictions = value, sample) %>%
#    dplyr::mutate(model = "loglin")
#  
# # Log cases / log deaths -----------------------------------------------------------------
# 
#  # Log adapted function
#  scoring_forecast_loglog <- ts_loglog_forecast(case_data = train_case_data,
#                                         deaths_data = train_death_data,
#                                         case_quantile = case_quantile,
#                                         sample_count = sample_count, 
#                                         horizon_weeks = horizon_weeks,
#                                         right_truncate_weeks = right_truncate_weeks,
#                                         format = FALSE)
#  
#  # Set up format for scoring
#  samples_loglog <- scoring_forecast_loglog %>% 
#    dplyr::ungroup() %>%
#    dplyr::mutate(epiweek = recode(epiweek, !!! setNames(test_date, scoring_date))) %>%
#    dplyr::select(epiweek, value, sample) %>%
#    dplyr::inner_join(test_data, by = "epiweek") %>%
#    dplyr::select(id = epiweek, 
#                  true_values = deaths, predictions = value, sample) %>%
#    dplyr::mutate(model = "loglog")
#  
# # Combine and score -----------------------------------------------------------------
# 
#  samples_all <- dplyr::bind_rows(samples_linear, samples_loglin, samples_loglog) 
#  score_combined <- scoringutils::eval_forecasts(data = samples_all) 
#  
