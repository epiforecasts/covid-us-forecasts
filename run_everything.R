
# ============================================================================ #
# Update data used for the forecasts
# ============================================================================ #
invisible(covidUS::get_us_deaths())


# update delays (not run)
# covidUS::upate_delays()

# ============================================================================ #
# Run all models, format submission, do plotting
# ============================================================================ #
# target_dates <- seq(lubridate::ymd('2020-06-01'), 
#                     lubridate::ymd(Sys.Date()), by = '1 day')

target_dates <- Sys.Date()

# run rt forecasts for target_dates
# current issue: forecasts will all be stored as latest I think --> only use for current date right now!
purrr::map(target_dates, covidUS::run_rt_forecast)

# run deaths-only forecasts for target_dates


# run deaths-on-cases forecasts for target_dates













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


