#' Compute inverse-score weights from per-model-date WIS scores
#'
#' For each forecast_date, computes weights as:
#'   weight_i = (1 / WIS_i) / sum(1 / WIS_j) for all models j
#'
#' @param scores A data.table with columns: model, forecast_date, wis
#' @return A data.table with columns: forecast_date, model, weight
#' @import data.table
compute_inverse_score_weights <- function(scores) {
  dt <- copy(scores)

  # Compute inverse WIS per model-date
  # Handle WIS == 0 by assigning a very large inverse (best possible score)
  dt[, inv_wis := ifelse(wis == 0, .Machine$double.xmax, 1 / wis)]

  # Normalise within each forecast_date so weights sum to 1
  dt[, weight := inv_wis / sum(inv_wis), by = forecast_date]

  # Clean up and return
  dt <- dt[, .(forecast_date, model, weight)]
  setorder(dt, forecast_date, model)
  return(dt)
}
