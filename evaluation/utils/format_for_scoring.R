#' Format forecast and observation data for use with scoringutils
#'
#' @param forecasts A data.table of raw forecasts (as read from submission CSVs)
#'   with columns: forecast_date, target, target_end_date, location, type,
#'   quantile, value, model
#' @param observations A data.table of observed values with columns:
#'   location, date, observed
#' @return A scoringutils forecast object ready for scoring
#' @import data.table
#' @import scoringutils
format_for_scoring <- function(forecasts, observations) {
  # Work on a copy
  dt <- copy(forecasts)

  # Keep only quantile rows (drop point forecasts)
  dt <- dt[type == "quantile"]

  # Parse horizon from target column (e.g. "1 wk ahead inc death" -> 1)
  dt[, horizon := as.integer(sub(" wk.*", "", target))]

  # Rename columns to scoringutils conventions
  setnames(dt, c("value", "quantile"), c("predicted", "quantile_level"))

  # Ensure quantile_level is numeric
  dt[, quantile_level := as.numeric(quantile_level)]

  # Ensure target_end_date is Date
  dt[, target_end_date := as.Date(target_end_date)]
  observations[, date := as.Date(date)]

  # Join observations on location + target_end_date
  dt <- merge(dt, observations,
              by.x = c("location", "target_end_date"),
              by.y = c("location", "date"),
              all.x = TRUE)

  # Drop rows with no observed data
  dt <- dt[!is.na(observed)]

  # Select final columns
  dt <- dt[, .(model, forecast_date, target_end_date, horizon,
               location, quantile_level, predicted, observed)]

  # Convert to scoringutils forecast object
  forecast_obj <- as_forecast_quantile(
    dt,
    forecast_unit = c("model", "forecast_date", "target_end_date",
                       "horizon", "location"),
    observed = "observed",
    predicted = "predicted",
    quantile_level = "quantile_level"
  )

  return(forecast_obj)
}
