# Forecast deaths from time-series
library(fable); library(forecastHybrid)
# Get data
# 
# Get data ----------------------------------------------------------------

# Source deaths and cases data; set to time-series
source(here::here("utils", "get_us_data.R"))

deaths <- get_us_deaths(data = "daily") %>%
  as_tsibble(key = state, index = date)

cases <- get_us_cases(data = "daily") %>%
  as_tsibble(key = state, index = date)


# Forecast A: Deaths auto-regressive ensemble -----------------------------
# Models: ETS; Seasonal naive; auto-Arima
combination <- data %>%
  fabletools::model(
    "model" = fabletools::combination_ensemble(
      fable::ETS(deaths ~ error() + trend() + season()), 
      fable::SNAIVE(deaths ~ lag(lag = "week")),
      fable::ARIMA(deaths ~ PDQ()),
      weights = "inv_var"))

forecast <- combination %>%
  forecast(h = 14)

quantile(forecast$.distribution[784], probs = 0.95)

quantile <- quantile(forecast$.distribution[1], probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
  tibble::enframe(name = "quantile", value = "value") %>%
  mutate(value = unlist(value),
         quantile = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))


# Forecast B: Deaths predicted by cases ensemble 
# Models: auto-Arima; Arima with lagged cases


# Forecast C: Deaths auto-regressive plus deaths predicted by cases

# Output quantiles



# Helpful code from covid-uk ----------------------------------------------


# Split into state data sets 

split_state <- function(df) {
  df <- df %>% 
    dplyr::arrange(state)
  
  df %>% 
    dplyr::group_split(state) %>% 
    setNames(unique(df$state)) %>% 
    purrr::map(~ dplyr::select(., -state))
}

state_obs <- deaths %>% 
  split_state()


# Set up parallel processing
if (!interactive()) {
  ## If running as a script enable this
  options(future.fork.enable = TRUE)
}

jobs <- length(state_obs)

plan(list(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
          tweak(multiprocess, workers = max(1, round(future::availableCores() / jobs)))))


# Fit model 
source(here::here("forecast/bed-forecast/utils/fit-fable-beds.R"))


# Run model for each state
state_forecasts <- furrr::future_map(names(state_obs),
                                             function(state) {
                                               message("Forecasting for: ", state)
                                               out <- fit_fable(observed = state_obs[[state]],
                                                                future = state_forecasts[[state]],
                                                                samples = 10, 
                                                                verbose = FALSE)
                                               
                                               return(out)
                                             })

names(state_forecasts) <- names(state_obs)

summarised_forecasts <- state_forecasts %>% 
  purrr::map(~.$summarised) %>% 
  dplyr::bind_rows(.id = "state")

forecast_date <- Sys.Date()

# Format for reporting ----------------------------------------------------
source(here::here("forecast/utils/format-for-reporting.R"))

csv_out <- summarised_forecasts %>% 
  dplyr::group_split(state) 

# Save forecasts  ---------------------------------------------------

dplyr::group_modify( ~ {
  quantile(.x$week_cases, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
    tibble::enframe(name = "quantile", value = "value") %>%
    dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)})

