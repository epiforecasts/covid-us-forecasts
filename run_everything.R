
# ============================================================================ #
# Update data used for the forecasts
# ============================================================================ #
source("utils/get_us_deaths.R")



# ============================================================================ #
# load in function to prepare submissions
# ============================================================================ #
source("utils/format_submission.R")




# update delays (not run)
# source("rt-forecast/update-delays.R")


# ============================================================================ #
# Run all models, format submission, do plotting
# ============================================================================ #


target_dates <- seq(ymd('2020-05-15'), ymd(Sys.Date()), by = '1 day')

for (t in target_dates) {
  target_date <- t
  # make new rt forecasts for US deaths
  source(here::here("rt-forecast", "update-us-deaths-rt.R"))
  
  # format submission
  forecast_date <- t
  source(here::here("utils", "update_report_submission.R"))
  
  dir.create(here::here("visualisation", t), showWarnings = FALSE)
  source(here::here("utils", "visualise_submission.R"))
  
  # visualise national death forecast
  national <- plot_forecasts(national = TRUE, states = NULL, cutoff = 25) + 
    theme(text = element_text(family = "Sans Serif"))
  
  ggsave(plot = national, path = here::here("visualisation", t), 
         filename = "death_national.png")
  
  # visualise subnational death forecast
  subnational <- plot_forecasts(national = FALSE, states = NULL, cutoff = 25) + 
    theme(text = element_text(family = "Sans Serif"))
  
  ggsave(plot = subnational, path = here::here("visualisation", t), 
         filename = "death_subnational.png", height = 30, width = 30)
}


