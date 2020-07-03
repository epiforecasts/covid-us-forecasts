# Log transform case forecast for use in death forecast
library(magrittr); library(dplyr); library(tidyr); library(EpiSoon); library(forecastHybrid)


ts_loglog_forecast <- function(case_data, deaths_data, case_quantile,
                                        sample_count, horizon_weeks, right_truncate_weeks,
                                        format = FALSE, quantiles_out = NULL){
  
# Set up case data and truncate
case_data_weekly_full <- case_data %>%
  mutate(epiweek = lubridate::epiweek(date),
         date = NULL) %>%
  group_by(state, epiweek) %>%
  summarise(cases = sum(cases)) 

right_truncate_date <- max(case_data_weekly_full$epiweek) - right_truncate_weeks

case_data_weekly <- case_data_weekly_full %>%
  filter(epiweek <= right_truncate_date) 


# Set up dates -----------------------------------------------------------

# Forecasting = next week + horizon_weeks
forecast_weeks <- seq(from = max(case_data_weekly$epiweek)+1, by = 1, length.out = horizon_weeks)
# Historical = last 6 weeks of data
historical_weeks <- case_data_weekly %>%
  ungroup() %>%
  filter(!epiweek %in% forecast_weeks) %>%
  select(epiweek) %>%
  filter(epiweek > max(epiweek) - 6) %>%
  unique() %>%
  pull(epiweek)



# Forecast cases ---------------------------------------------------------

case_forecast <- case_data_weekly %>%
  group_by(state) %>%
  group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, epiweek %in% historical_weeks) %>% 
                                                 pull("cases") %>%
                                                 log(),
                                               samples = sample_count, 
                                               horizon = horizon_weeks,
                                               model_params = list(models = "aez", weights = "equal",
                                                                   a.args = list()),
                                               forecast_params = list(PI.combination = "mean"))) %>%
  mutate(sample = rep(1:sample_count)) %>%
  pivot_longer(cols = starts_with("..."), names_to = "epiweek")

# Get quantile
quantile <- case_forecast %>%
  group_by(state, epiweek) %>%
  group_modify( ~ as.data.frame(quantile(.x$value, probs = case_quantile, na.rm = T))) %>%
  mutate(quantile = case_quantile) %>%
  ungroup()

# Format
dates_from <- unique(quantile$epiweek)

case_forecast <- quantile %>%
  select(state, epiweek, "cases" = 3) %>%
  mutate(epiweek = recode(epiweek, !!! setNames(forecast_weeks, dates_from)),
         cases = ifelse(cases < 0, 0, cases),
         cases = round(cases))


# Forecast deaths ---------------------------------------------------------

# Set deaths data and truncate
deaths_data_weekly_full <- deaths_data %>%
  mutate(epiweek = lubridate::epiweek(date),
         date = NULL) %>%
  group_by(state, epiweek) %>%
  summarise(deaths = sum(deaths))

deaths_data_weekly <- deaths_data_weekly_full %>%
  filter(epiweek <= right_truncate_date)

# Join deaths and cases
log_cases <- mutate(case_data_weekly, cases = log(cases))
cases_deaths <- bind_rows(log_cases, case_forecast) %>%
  full_join(deaths_data_weekly, by = c("state", "epiweek"))

# Forecast deaths (y) using cases
death_forecast <- cases_deaths %>%
  group_by(state) %>%
  group_modify(~ EpiSoon::forecastHybrid_model(y = filter(.x, epiweek %in% historical_weeks) %>%
                                                 pull("deaths") %>%
                                                 log(),
                                               samples = sample_count, 
                                               horizon = horizon_weeks,
                                               model_params = list(models = "aez", weights = "equal",
                                                                   a.args = list(
                                                                     xreg = filter(.x, epiweek %in% historical_weeks) %>%
                                                                       pull("cases"))
                                               ),
                                               forecast_params = list(xreg = filter(.x, epiweek %in% forecast_weeks) %>%
                                                                        pull("cases"),
                                                                      PI.combination = "mean")
  )) %>%
  mutate(sample = rep(1:sample_count)) %>%
  pivot_longer(cols = starts_with("..."), names_to = "epiweek") %>%
  mutate(value = exp(value))



# Return forecast ---------------------------------------------------------

if(format == FALSE){
  return(death_forecast)
}

if(format == TRUE){
  # Get quantiles
  quantile <- death_forecast %>%
    group_by(state, epiweek) %>%
    group_modify( ~ as.data.frame(quantile(.x$value, probs = quantiles_out, na.rm = T))) %>%
    mutate(quantile = quantiles_out) %>%
    ungroup() %>%
    select(state, epiweek_target = epiweek, quantile, "deaths" = 3) %>%
    mutate(epiweek_target = recode(epiweek_target, !!! setNames(forecast_weeks, dates_from)),
           deaths = ifelse(deaths < 0, 0, deaths),
           deaths = round(deaths),
           model_type = "log_deaths_on_log_cases",
           date_created = Sys.Date())
  
  return(quantile)
}
}
