# Score timeseries forecasts
library(magrittr); library(dplyr); library(tidyr); library(scoringutils)

# Function to produce CRPS and log scoring --------------------------------
score_timeseries <- function(data, sample_count, models, day_lag){
  # Split into observed and future datasets
  train_data <- data %>% 
    dplyr::filter(date <= (max(date) - lubridate::days(day_lag)))
  
  test_data <- data %>% 
    dplyr::anti_join(train_data, by = "date") %>% 
    dplyr::mutate(sample = 1) %>% 
    dplyr::mutate(h = 1:dplyr::n()) %>% 
    dplyr::filter(h <= day_lag)
  
  # Forecast
  scoring_forecast <- timeseries_death_forecast(data = train_data, 
                                                sample_count = sample_count, 
                                                horizon_days = day_lag,
                                                models = models, 
                                                format = FALSE)
  
  test_date <- unique(test_data$date)
  scoring_date <- unique(scoring_forecast$date)
  
  samples <- scoring_forecast %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(date = recode(date, !!! setNames(test_date, scoring_date))) %>%
    dplyr::select(date, value, sample) %>%
    dplyr::inner_join(test_data %>% 
                        dplyr::select(date), by = "date")
  
  # Score
  score <- scoringutils::eval_forecasts_prob_cont(true_values = test_data$deaths, 
                                                 predictions = list(samples %>% 
                                                                      tidyr::spread(key = "sample", value = "value") %>% 
                                                                      dplyr::select(-date) %>% 
                                                                      as.matrix()
                                                 ))
  return(score)
}


# Score new forecasts ---------------------------------------------------------
# Get functions and data
source(here::here("timeseries-forecast", "timeseries-death-forecast.R"))
source(here::here("utils", "get-us-data.R"))

# National forecast scores
data_national <- get_us_deaths(data = "daily") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(state = "US")
score_national <- score_timeseries(
  # Set forecasting parameters
  data = data_national,
  sample_count = 1000,
  models = "aez",
  # Set scoring parameters
  day_lag = 5)

# State forecast scores
data_state <- get_us_deaths(data = "daily") 
score_state <- data_state %>%
  mutate(state1 = state) %>%
  group_by(state1) %>%
  group_modify( ~ score_timeseries(data = .x,
                                   sample_count = 10,
                                   models = "aez",
                                   day_lag = 5)) %>%
  mutate(score = rep(c("crps", "logs")))


