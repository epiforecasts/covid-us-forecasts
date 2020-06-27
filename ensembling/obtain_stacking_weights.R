# ============================================================================ #
# load all models vs. true values
# ============================================================================ #

files <- list.files(here::here("evaluation", "observed_vs_forecast"))

# load in (toy) data
files <- files[-1]

# combine data and rename columns
df <- purrr::map_dfr(files, 
                     function(file) {
                       data.table::fread(here::here("evaluation", "observed_vs_forecast", file))
                     }) %>%
  dplyr::rename(y_obs = true_values, 
                y_pred = predictions, 
                geography = region, 
                date = id, 
                sample_nr = sample) 

# obtain stacking weights
h = 7 # horizon to do stacking on
stacking_weights <- df %>%
  # dplyr::filter(horizon == 7) %>%
  stackr::crps_weights(lambda = "equal")

weights <- data.frame(stacking_weights = stacking_weights, 
                      model = unique(df$model))

data.table::fwrite(weights, here::here("ensembling", "stacking_weights.csv"))

