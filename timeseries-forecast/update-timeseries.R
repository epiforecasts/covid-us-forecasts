# Update deaths forecasts from timeseries
library(magrittr); library(dplyr)

# Get functions and data
source(here::here("timeseries-forecast", "timeseries-death-forecast.R"))
source(here::here("utils", "get_us_data.R"))

data_state <- get_us_deaths(data = "daily")

# Set parameters
sample_count = 10
horizon_days = 7
models = "aez"
quantiles_out <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

# State forecast
state_forecast <- timeseries_death_forecast(data = data_state, 
                                            sample_count = sample_count, 
                                            horizon_days = horizon_days,
                                            models = models, 
                                            format = TRUE,
                                            quantiles_out = quantiles_out)

# National forecast
data_national <- data_state %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(state = "US")

national_forecast <- timeseries_death_forecast(data = data_national, 
                                               sample_count = sample_count, 
                                               horizon_days = horizon_days,
                                               models = models, 
                                               format = TRUE,
                                               quantiles_out = quantiles_out)
