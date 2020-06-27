#' @title Create Toy Data
#' @details
#' Creates toy data with some predictions vs. true values
#' 
#' @param forecast_dates dates for which to create forecasts
#' @param y_true allows to pass a vector with true values that can be used to 
#' replace the randomly generated ones. Useful if you want true values to match
#' across different toy data sets
#' @param horizons horizons to "forecast"
#' @param regions names of regions
#' @param samples number of samples 
#' @param models names for the toy models 
#' 
#' @return data.frame with toy predictions
#'
#' @export
#' @examples
#' # make toy 1
#' toy1 <- make_toy_data(samples = 500, 
#'                       models = "model1")
#' data.table::fwrite(toy1, here::here("evaluation", "observed_vs_forecast", "toy1-forecast-vs-obs.csv"))
#' 
#' # make toy 2
#' true_values <- toy1$true_values
#' toy2 <- make_toy_data(samples = 500, 
#'                       models = "model2", 
#'                       y_true = true_values)
#' #write toy data to file
#' data.table::fwrite(toy2, here::here("evaluation", "observed_vs_forecast", "toy2-forecast-vs-obs.csv"))
#' 




# create toy data and save as csv
make_toy_data <- function(forecast_dates = NULL, 
                          y_true = NULL, 
                          horizons = 1:10, 
                          regions = c("US", "Alabama"), 
                          samples = 5, 
                          models = c("model1", "model2")) {
  
  if(is.null(forecast_dates)) {
    forecast_dates <- c(as.Date("2025-01-01"), as.Date("2025-01-02"))
  }
  
  sample <- 1:samples
  
  toydata <- tidyr::expand_grid(forecast_dates, horizons, regions, sample, models) %>%
    dplyr::rename(forecast_date = forecast_dates, 
                  horizon = horizons, 
                  region = regions, 
                  model = models) %>%
    dplyr::mutate(id = forecast_date + lubridate::days(horizon),
                  week = lubridate::floor_date(id, unit = "week", week_start = 7)) 
  
  if(!is.null(y_true)) {
    toydata %>%
      dplyr::mutate(true_values = y_true) %>%
      dplyr::group_by(forecast_date, region, id, model) %>%
      dplyr::mutate(predictions = rpois(samples, lambda = unique(true_values))) %>%
      dplyr::ungroup()
    
  } else {
    toydata %>%
      dplyr::group_by(forecast_date, region, id, model) %>%
      dplyr::mutate(true_values = rpois(1, 100), 
                    predictions = rpois(samples, lambda = unique(true_values))) %>%
      dplyr::ungroup()
  }
}

