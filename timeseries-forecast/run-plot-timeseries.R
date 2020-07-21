# Saves plots for timeseries forecasts
# Plots saved to "timeseries-forecast/figures"

source(here::here("timeseries-forecast", "plot-timeseries-fn.R"))

# Show whole timeseries
plot_timeseries(model_type = "deaths-only", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = 10, id = "all-time-")
plot_timeseries(model_type = "deaths-on-cases", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = 10, id = "all-time-")

# Show last few weeks
recent = lubridate::epiweek(Sys.Date()) - 8

plot_timeseries(model_type = "deaths-only", date = "latest", right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")
plot_timeseries(model_type = "deaths-on-cases", date = "latest", right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")

# Today's forecast
date <- as.character(Sys.Date())

plot_timeseries(model_type = "deaths-only", date = date, right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")

plot_timeseries(model_type = "deaths-on-cases", date = date, right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")

