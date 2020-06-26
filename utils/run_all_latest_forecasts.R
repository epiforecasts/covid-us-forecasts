
# ============================================================================ #
# Update data used for the forecasts
# ============================================================================ #

source(here::here("R", "get_us_data.R"))


# update delays (not run)
# source(here::here("rt-forecast", "update-delays.R"))
# update_delays()

# ============================================================================ #
# Run all models
# ============================================================================ #

# run rt forecast for current date
source(here::here("rt-forecast", "update-us-deaths-rt.R"))

# run timeseries forecasts
source(here::here("timeseries-forecast", "update-timeseries.R"))


# ============================================================================ #
# format submission
# ============================================================================ #

# source functions
source(here::here("R", "format_submission.R"))

# format timeseries forecasts
source(here::here("utils", "format-submission-timeseries.R"))

# format rt forecasts
source(here::here("utils", "update_report_submission.R"))


# ============================================================================ #
# ensembling
# ============================================================================ #

source(here::here("ensemble-forecast", "qra-ensemble", "quantile_ensembling.R"))

# ============================================================================ #
# plotting
# ============================================================================ #








# for (t in target_dates) {
#   target_date <- t
#   # make new rt forecasts for US deaths
#   source(here::here("rt-forecast", "update-us-deaths-rt.R"))
#   
#   # format submission
#   forecast_date <- t
#   source(here::here("utils", "update_report_submission.R"))
#   
#   dir.create(here::here("visualisation", t), showWarnings = FALSE)
#   source(here::here("utils", "visualise_submission.R"))
#   
#   # visualise national death forecast
#   national <- plot_forecasts(national = TRUE, states = NULL, cutoff = 25) + 
#     theme(text = element_text(family = "Sans Serif"))
#   
#   ggsave(plot = national, path = here::here("visualisation", t), 
#          filename = "death_national.png")
#   
#   # visualise subnational death forecast
#   subnational <- plot_forecasts(national = FALSE, states = NULL, cutoff = 25) + 
#     theme(text = element_text(family = "Sans Serif"))
#   
#   ggsave(plot = subnational, path = here::here("visualisation", t), 
#          filename = "death_subnational.png", height = 30, width = 30)
# }
# 
# 
