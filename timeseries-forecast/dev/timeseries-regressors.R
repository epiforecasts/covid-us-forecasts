# Forecast deaths with case regressor
# library(dplyr); library(forecastHybrid); library(EpiSoon)
# source(here::here("timeseries-forecast", "timeseries-death-forecast.R"))

# Get functions and set global parameters ---------------------------------


source(here::here("timeseries-forecast", "timeseries-case-forecast.R"))


sample_count = 10
horizon_days = 14
models = "aez"
single_quantile = 0.5

## This function is never called but it looks like the output is used below. I think its a wrapper we don't need
forecast_case_regressor <- function(data, sample_count, horizon_days, models, single_quantile){
  
# Get cases ------------------------------------------------------
source(here::here("utils", "get-us-data.R"))


# Forecast cases ----------------------------------------------------------------

# Set parameters and forecast
case_forecast <- timeseries_case_forecast(data = cases, 
                                             sample_count = sample_count, 
                                             horizon_days = horizon_days,
                                             models = models, 
                                             format = TRUE,
                                             quantiles_out = single_quantile)
# Get forecast
# case_forecast_mid <- case_forecast %>%
#   dplyr::filter(quantile == 0.5) %>%
#   dplyr::select(date, state, cases)

# Convert to formats for use in forecastHybrid
case_newxreg <- as.vector(case_forecast$cases)

return(case_newxreg)

}



# Get deaths ---------------------------------------------------

deaths_national <- get_us_deaths(data = "daily") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(state = "US")


# Forecast deaths ---------------------------------------------------------

# Set parameters & forecast

death_forecast <- timeseries_death_forecast(data = deaths_national, 
                                          sample_count = sample_count, 
                                          horizon_days = horizon_days,
                                          models = models, 
                                          xreg = case_xreg,
                                          newxreg = case_newxreg,
                                          format = TRUE,
                                          quantiles_out = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))

death_forecast <- data %>%
  group_by(state) %>%
  group_modify(~ EpiSoon::forecastHybrid_model(y = .x$deaths,
                                               samples = sample_count, 
                                               horizon = horizon_days,
                                               model_params = list(models = models,
                                                                   a.args = list(xreg = xreg)),
                                               forecast_params = list(xreg = newxreg))) %>%
  mutate(sample = rep(1:sample_count)) %>%
  pivot_longer(cols = starts_with("..."), names_to = "date")
  























# Get functions and data
source(here::here("timeseries-forecast", "timeseries-case-forecast.R"))
cases_state <- get_us_cases(data = "daily")

state_xreg <- as.matrix(cases_state[,c("state", "cases")])

# State case forecast
state_case_forecast <- timeseries_case_forecast(data = cases_state, 
                                                sample_count = sample_count, 
                                                horizon_days = horizon_days,
                                                models = models, 
                                                format = TRUE,
                                                quantiles_out = 0.5)

state_newxreg <- as.matrix(state_case_forecast[,c("state", "cases")])

# State death forecast
death_forecast <- timeseries_death_forecast(data = deaths_state, 
                                            sample_count = sample_count, 
                                            horizon_days = horizon_days,
                                            models = models, 
                                            xreg = state_xreg,
                                            newxreg = state_newxreg,
                                            format = TRUE,
                                            quantiles_out = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))

death_forecast <- deaths_state %>%
  group_by(state) %>%
  group_modify(~ timeseries_death_forecast(data = deaths_state, 
                                           sample_count = sample_count, 
                                           horizon_days = horizon_days,
                                           models = models, 
                                           xreg = state_xreg,
                                           newxreg = state_newxreg,
                                           format = TRUE,
                                           quantiles_out = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))) %>%
  mutate(sample = rep(1:sample_count)) %>%
  pivot_longer(cols = starts_with("..."), names_to = "date")