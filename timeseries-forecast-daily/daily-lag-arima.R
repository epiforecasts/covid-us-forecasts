library(magrittr); library(dplyr)

# Using US national

case_data_lags <- cases_national %>%
  mutate(lag1 = lag(cases, 1),
         lag2 = lag(cases, 2),
         lag3 = lag(cases, 3),
         lag4 = lag(cases, 4),
         lag5 = lag(cases, 5),
         lag6 = lag(cases, 6),
         lag7 = lag(cases, 7),
         lag8 = lag(cases, 8),
         lag9 = lag(cases, 9),
         lag10 = lag(cases, 10),
         lag11 = lag(cases, 11),
         lag12 = lag(cases, 12),
         lag13 = lag(cases, 13),
         lag14 = lag(cases, 14)) %>%
  select(cases, starts_with("lag"))

case_data_lags <- as.matrix(case_data_lags)

arima_lag <- forecast::auto.arima(deaths_national$deaths, xreg = case_data_lags)

jtools::plot_summs(arima_lag)
ggplot2::ggsave(filename = "timeseries-daily-forecast/deaths-on-cases-daily-lag.png")

arima_lag_tidy <- broom::tidy(arima_lag)
arima_lag_tidy$estimate <- abs(arima_lag_tidy$estimate)
