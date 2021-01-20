# Packages ----------------------------------------------------------------
library(dplyr)
library(here)

# forecast date -----------------------------------------------------------
forecast_date <- readRDS(here("data", "target_date.rds"))

# Set up functions and data -----------------------------------------------
source(here::here("utils", "get-us-data.R"))
source(here::here("models", "timeseries", "utils", "deaths-on-cases-forecast.R"))

# Get data
weekly_deaths_state <- get_us_deaths(data = "daily") %>%
  filter(date >= (as.Date(forecast_date) - 8 * 4)) %>%
  group_by(state, epiweek) %>%
  summarise(deaths = sum(deaths), 
            target_end_date = max(date),
            .groups = "drop_last") %>%
  dplyr::select(-epiweek) %>%
  dplyr::ungroup()

weekly_deaths_national <- weekly_deaths_state %>%
  group_by(target_end_date) %>%
  summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  mutate(state = "US")

obs <- dplyr::bind_rows(weekly_deaths_state, weekly_deaths_national) %>%
  dplyr::filter(target_end_date <= as.Date(forecast_date))

# get median and sd of forecast ------------------------------------------------
median <- obs %>%
  dplyr::filter(target_end_date == max(target_end_date)) %>%
  dplyr::group_by(state) %>%
  dplyr::rename(median = deaths) %>%
  dplyr::mutate(horizon = list(1:4), 
                median = list(rep(median, 4))) %>%
  tidyr::unnest(cols = c(horizon, median))

sigma <-  obs %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(difference = c(NA, diff(log(deaths + 0.0001)))) %>%
  dplyr::filter(target_end_date > max(target_end_date) - 4 * 7) %>%
  dplyr::summarise(sd = sd(difference, na.rm = TRUE), 
                   .groups = "drop_last") %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(horizon = list(1:4), 
                sd = list(sd * c(0.8, 0.9, 1, 1.1))) %>%
  tidyr::unnest(cols = c(horizon, sd)) %>%
  dplyr::ungroup()

combined <- dplyr::inner_join(median, sigma, by = c("state", "horizon")) %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date) + 7 * horizon)


# get quantiles for forecast ---------------------------------------------------
quantile_grid <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

forecasts <- combined %>%
  dplyr::rowwise() %>%
  dplyr::mutate(quantile = list(quantile_grid), 
                value = list(exp(qnorm(p = quantile_grid, 
                                       mean = log(median), 
                                       sd = sd))))


# do formatting for submission -------------------------------------------------
formatted_forecasts <- 0

state_codes <- readRDS(here::here("data", "state_codes.rds"))

formatted <- dplyr::left_join(forecasts, state_codes, by = "state") %>%
  dplyr::mutate(forecast_date = forecast_date, 
                submission_date = forecast_date, 
                target = paste(horizon, "wk ahead inc death"), 
                type = "quantile") %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(median, sd, state)) %>%
  tidyr::unnest(cols = c(quantile, value))

# add point forecast
point_forecast <- formatted_forecasts %>%
  dplyr::filter(quantile == 0.5) %>%
  dplyr::mutate(type = "point")

formatted_forecasts <- dplyr::bind_rows(formatted, 
                                        point_forecast) %>%
  dplyr::relocate(forecast_date, submission_date, target, target_end_date, location, type, quantile, value)


submission_dir <- here("models", "constant-baseline", "data", "submission")
if (!dir.exists(submission_dir)) {
  dir.create(submission_dir, recursive = TRUE)
}

data.table::fwrite(formatted_forecasts, paste0(submission_dir, "/", forecast_date, ".csv"))