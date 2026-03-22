# Score all past forecasts using scoringutils
#
# Reads all 97 historical submission CSVs, loads truth data from JHU,
# formats for scoringutils, computes WIS and other proper scoring rules,
# and saves summary tables to evaluation/data/.
#
# Usage: Rscript evaluation/score-forecasts.R

library(data.table)
library(here)
library(scoringutils)

# Source utilities
source(here("utils", "get-us-data.R"))
source(here("evaluation", "utils", "format_for_scoring.R"))
source(here("evaluation", "utils", "inverse_score_weights.R"))

# ---- Step 1: Load all forecasts ----
message("Loading all forecast submissions...")
submission_files <- list.files(here("submissions", "all-models"),
                               pattern = "\\.csv$", full.names = TRUE)
forecasts <- rbindlist(lapply(submission_files, fread), use.names = TRUE)
message(sprintf("Loaded %d forecast rows from %d files",
                nrow(forecasts), length(submission_files)))

# ---- Step 2: Load truth data ----
truth_cache <- here("evaluation", "data", "truth.csv")

if (file.exists(truth_cache)) {
  message("Loading cached truth data...")
  observations <- fread(truth_cache)
} else {
  message("Fetching truth data from JHU...")
  deaths <- get_us_data("deaths",
                        include_national = TRUE,
                        incident = TRUE,
                        incident_weekly = TRUE)

  # Convert to data.table
  deaths <- as.data.table(deaths)

  # Keep only Saturday epiweek end dates (where value_weekly is populated)
  observations <- deaths[!is.na(value_weekly) & weekdays(date) == "Saturday",
                          .(state, date, observed = value_weekly)]

  # Join with state codes to get FIPS location codes
  state_codes <- readRDS(here("data", "state_codes.rds"))
  observations <- merge(observations, state_codes, by = "state", all.x = TRUE)

  # For US national, set location code
  observations[state == "US", location := "US"]
  observations <- observations[!is.na(location), .(location, date, observed)]

  # Cache for future runs
  fwrite(observations, truth_cache)
  message(sprintf("Saved truth data: %d observations", nrow(observations)))
}

# ---- Step 3: Format and score ----
message("Formatting data for scoringutils...")
forecast_obj <- format_for_scoring(forecasts, observations)
message(sprintf("Scoring %d forecast-observation pairs...", nrow(forecast_obj)))

scored <- score(forecast_obj)
message("Scoring complete.")

# ---- Step 4: Summarise and save ----
output_dir <- here("evaluation", "data")

# By model
scores_model <- summarise_scores(scored, by = "model")
fwrite(scores_model, file.path(output_dir, "scores_by_model.csv"))
message("Saved scores_by_model.csv")

# By model + horizon
scores_horizon <- summarise_scores(scored, by = c("model", "horizon"))
fwrite(scores_horizon, file.path(output_dir, "scores_by_model_horizon.csv"))
message("Saved scores_by_model_horizon.csv")

# By model + location
scores_location <- summarise_scores(scored, by = c("model", "location"))
fwrite(scores_location, file.path(output_dir, "scores_by_model_location.csv"))
message("Saved scores_by_model_location.csv")

# By model + forecast_date
scores_date <- summarise_scores(scored, by = c("model", "forecast_date"))
fwrite(scores_date, file.path(output_dir, "scores_by_model_date.csv"))
message("Saved scores_by_model_date.csv")

# ---- Step 5: Compute inverse-score weights ----
message("Computing inverse-score weights...")
weights <- compute_inverse_score_weights(scores_date)
fwrite(weights, file.path(output_dir, "inverse_score_weights.csv"))
message("Saved inverse_score_weights.csv")

message("Done. All scoring outputs saved to evaluation/data/")
