# Saves plots for timeseries forecasts
# Figures saved to "timeseries-forecast/figures"

source(here::here("timeseries-forecast", "plot-timeseries-fn.R"))

# Show whole timeseries
plot_timeseries(model_type = "deaths-only", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = 10)
plot_timeseries(model_type = "deaths-on-cases", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = 10)

# Show last three weeks
recent = lubridate::epiweek(Sys.Date()) - 7

plot_timeseries(model_type = "deaths-only", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")
plot_timeseries(model_type = "deaths-on-cases", date = "latest-weekly", right_truncate_weeks = 1, 
                xlim_min = recent, id = "recent-")
