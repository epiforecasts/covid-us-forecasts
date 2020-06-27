# ============================================================================ #
# load all models vs. true values
# ============================================================================ #

files <- list.files(here::here("evaluation", "observed_vs_forecast"))

# load in (toy) data
files <- files[-1]

# combine data
df <- purrr::map_dfr(files, 
                     function(file) {
                       data.table::fread(here::here("evaluation", "observed_vs_forecast", file))
                     })


# ============================================================================ #
# evaluate forecasts
# ============================================================================ #

# do automatic scoring with scoringutils
scored_forecasts <- df %>%
  dplyr::group_by(horizon, region) %>%
  tidyr::nest() %>%
  dplyr::mutate(scores = purrr::map(data, 
                                    function(x) {
                                      scoringutils::eval_forecasts(x, summarised = TRUE)
                                      })) %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::group_by(forecast_date, id, region) %>%
  dplyr::summarise(scores = unique(scores)) %>%
  tidyr::unnest(cols = c(scores)) %>%
  dplyr::mutate(horizon = as.numeric(as.Date(id) - as.Date(forecast_date)))

# also add median and iqr and ci
# this is done to use pre-existing plotting functions
missing_scores <- df %>%
  dplyr::group_by(forecast_date, id, region, model) %>%
  dplyr::summarise(median = median(predictions), 
                   iqr = IQR(predictions), 
                   ci = scoringutils::interval_score(true_values = predictions, 
                                                     lower = quantile(predictions, prop = 0.025), 
                                                     upper = quantile(predictions, prop = 0.975), 
                                                     95))

# join together to obtain all scores and forecasts
full <- dplyr::inner_join(scored_forecasts, missing_scores) %>%
  dplyr::rename(dss = DSS, 
                crps = CRPS, 
                calibration = pit_p_val) %>%
  dplyr::mutate(logs = NA)

# summarise scores
summarised_scores <- EpiSoon::summarise_scores(full)



# ============================================================================ #
# plot evaluation
# ============================================================================ #

# use pre-existing functions


