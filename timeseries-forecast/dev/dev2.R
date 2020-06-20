# Exploring time-series models for US deaths
library(dplyr); library(fable); library(feasts)

# Get data ----------------------------------------------------------------

# Source deaths data and set to timeseries
# Grouping to US national for exploring

source(here::here("utils", "get_us_data.R"))
deaths <- get_us_deaths(data = "daily") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  as_tsibble()



# Explore time-series -----------------------------------------------------

## Check decomposition
deaths %>%
  model(STL(deaths ~ trend(window=7) + season(window='periodic'),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

## See how models fit independently
separate <- deaths %>%
  fabletools::model(
    "ets" = fable::ETS(deaths ~ error() + trend() + season()), 
    "arima" = fable::ARIMA(deaths ~ PDQ()),
    "snaive" = fable::SNAIVE(deaths ~ lag(lag = "week"))
    )
separate_fc <- forecast(separate, h = 28)
separate_fc %>%
  autoplot(deaths)

# Ensemble models
## Model and forecast
combination <- deaths %>%
  fabletools::model(
    "model" = fabletools::combination_ensemble(
      fable::ETS(deaths ~ error() + trend() + season()), 
      fable::SNAIVE(deaths ~ lag(lag = "week")),
      fable::ARIMA(deaths ~ PDQ()),
      weights = "inv_var"))
combination_fc <- forecast(combination, h = 28)

## Plot
combination_fc %>%
  autoplot(deaths_us) +
  ggplot2::ylim(0,3000)

# Test
## Last 10 days of data
test <- deaths_us %>%
  filter(date > (max(date) - 10))
train <- deaths_us %>%
  filter(date <= (max(date) - 10))

combination <- train %>%
  fabletools::model(
    "model" = fabletools::combination_ensemble(
      fable::ETS(deaths ~ error() + trend() + season()), 
      fable::SNAIVE(deaths ~ lag(lag = "week")),
      fable::ARIMA(deaths ~ PDQ()),
      weights = "inv_var"))


combination_fc <- forecast(combination, h = 10) %>%
  accuracy(deaths)

accuracy(separate_fc, test)



# EpiSoon::forecastHybrid_model
# # model_params: forecastHybrid::hybridModel
# # forecast_params: forecastHybrid:::forecast.hybridModel
library(forecastHybrid)
# Default
ex1 <- EpiSoon::forecastHybrid_model(y = deaths$deaths,
                                     samples = 1, horizon = 7,
                                     model_params = list(models = "zeta"))


## Cross-validation for error weights
ex2 <- EpiSoon::forecastHybrid_model(y = deaths$deaths,
                                     samples = 1, horizon = 7,
                                     model_params = list(cvHorizon = 7, windowSize = 7,
                                                         rolling = TRUE, models = "zeta"))

## Used for forcasting with non-default arguments
ex3 <- EpiSoon::forecastHybrid_model(y = deaths$deaths,
                                     horizon = 7,
                                     model_params = list(models = "zeta", weights = "equal"),
                                     forecast_params = list(PI.combination = "mean"))


ex4 <- EpiSoon::forecastHybrid_model(
  y = y[max(1, length(y) - 21):length(y)],
  horizon = 21,
  samples = 10,
  model_params = list(models = "aefz", weights = "equal"),
  forecast_params = list(PI.combination = "mean"))

med = median(ex4$...1)
# ex1 782 ex2 728 ex3 818 ex4 687
c99 = quantile(ex4$...1, prob = 0.99, na.rm = TRUE)


##### Fit fable
observed <- deaths[1:(nrow(deaths)-10),]
future <- deaths %>%
  dplyr::anti_join(observed, by = "date") %>%
  dplyr::mutate(sample = 1)
wrap_models <- function(data) {
  data %>%
    fabletools::model(
      "model" = fabletools::combination_ensemble(
        fable::ETS(deaths ~ error() + trend() + season()), 
        fable::SNAIVE(deaths ~ lag(lag = "week")),
        fable::ARIMA(deaths ~ PDQ()),
        weights = "inv_var"))
}


out <- fit_fable(observed = observed, 
                 future = future, 
                 samples = 1000, 
                 model = wrap_models, 
                 verbose = TRUE)

# fit-fable ---------------------------------------------------------------



safe_forecast <- purrr::safely(fabletools::forecast)
forecast_results <- furrr::future_map(deaths, 
                                      ~ safe_forecast(fit)[[1]], 
                                      .progress = TRUE)
forecast_results <- purrr::compact(forecast_results)

samples <- furrr::future_map_dfr(forecast_results,
                                 ~ tibble::tibble(
                                   date = .$date,
                                   target = purrr::map(.$.distribution, 
                                                       ~ as.integer(rnorm(samples, mean = .$mean, 
                                                                          sd = .$sd))),
                                   sample = list(1:samples)) %>% 
                                   tidyr::unnest(cols = c("target", "sample")) %>% 
                                   dplyr::mutate(target = ifelse(target > 0, target, 0)), 
                                 .progress = TRUE)


forecast_sum <- samples %>% 
  dplyr::group_by(date) %>%
  dplyr::summarise(
    med = median(target),
    c99 = quantile(target, prob = 0.99, na.rm = TRUE),
    c95 = quantile(target, prob = 0.95, na.rm = TRUE),
    c75 = quantile(target, prob = 0.75, na.rm = TRUE),
    c25 = quantile(target, prob = 0.25, na.rm = TRUE),
    c5 = quantile(target, prob = 0.05, na.rm = TRUE),
    c1 = quantile(target, prob = 0.01, na.rm = TRUE),
    c10 = quantile(target, prob = 0.1, na.rm = TRUE),
    c15 = quantile(target, prob = 0.15, na.rm = TRUE),
    c20 = quantile(target, prob = 0.20, na.rm = TRUE), 
    c30 = quantile(target, prob = 0.30, na.rm = TRUE),
    c35 = quantile(target, prob = 0.35, na.rm = TRUE),
    c40 = quantile(target, prob = 0.40, na.rm = TRUE),
    c45 = quantile(target, prob = 0.45, na.rm = TRUE),
    c50 = quantile(target, prob = 0.50, na.rm = TRUE), 
    c55 = quantile(target, prob = 0.55, na.rm = TRUE),
    c60 = quantile(target, prob = 0.60, na.rm = TRUE),
    c65 = quantile(target, prob = 0.65, na.rm = TRUE),
    c70 = quantile(target, prob = 0.70, na.rm = TRUE),
    c80 = quantile(target, prob = 0.80, na.rm = TRUE),
    c85 = quantile(target, prob = 0.85, na.rm = TRUE), 
    c90 = quantile(target, prob = 0.90, na.rm = TRUE)
  )
out <- list()
out$fit <- fit
out$forecast <- forecast_results
out$samples <- samples
out$summarised <- forecast_sum

#
# 
# source(here::here("utils", "fit-fable.R"))
# source(here::here("utils", "compare-models.R"))
# 
compare_models(obs_data = observed, day_lag = 5, models = "ETS", max_horizon = 7)
# 


