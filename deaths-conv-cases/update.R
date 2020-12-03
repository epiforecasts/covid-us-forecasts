# Packages -----------------------------------------------------------------
library(EpiNow2, quietly = TRUE)
library(data.table, quietly = TRUE)
library(here, quietly = TRUE)
library(future, quietly = TRUE)
library(devtools, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(purrr, quietly = TRUE)

# Set target date ---------------------------------------------------------
target_date <- as.character(Sys.Date()) 

# Get Observations --------------------------------------------------------
# assumes no missing data (either explicit or implicit)
source(here("utils", "get-us-data.R"))
deaths_raw <- get_us_deaths(data = "daily")
deaths_raw <- as.data.table(deaths_raw)
observations <- deaths_raw[, .(region = state, date, secondary = deaths)]
us_obs <- copy(observations)[, .(secondary = sum(secondary, na.rm = TRUE)), by = "date"]
us_obs <- us_obs[, region := "US"]
observations <- rbindlist(list(us_obs, observations), use.names = TRUE)
observations <- observations[date <= as.Date(target_date)]
observations <- observations[date >= (max(date) - 12*7)]
setorder(observations, region, date)

# Get case forecasts ------------------------------------------------------
# load nowcast/forecast
case_forecast <- suppressWarnings(
  get_regional_results(results_dir = here("rt-forecast-2", "forecast", "deaths_forecast",
                                          "fixed_rt", "regional"),
                       date = ymd(target_date),
                       forecast = TRUE, samples = TRUE)$estimates$samples)
# extracte infections
case_forecast <- case_forecast[variable == "infections"]
case_forecast <- case_forecast[, .(region, date, sample, value)]

# summarise for observations
obs_inf <- copy(case_forecast)
obs_inf <- obs_inf[, .(primary = median(value)), by = c("region", "date")]
setorder(obs_inf, region, date)

# link to observed deaths
merge(observations, obs_inf, by = c("region", "date"))

# Forecast deaths from cases ----------------------------------------------
# set up parallel options
plan("multiprocess")

# load the prototype regional_secondary function
source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

# run across Poland and Germany specifying options for estimate_secondary (EpiNow2)
forecast <- regional_secondary(observations, case_forecast,
                               delays = delay_opts(list(mean = 3, mean_sd = 0.5, 
                                                        sd = 0.4, sd_sd = 0.1, max = 30)),
                               secondary = secondary_opts(type = "incidence"),
                               obs = obs_opts(scale = list(mean = 0.005, sd = 0.0025)),
                               burn_in = as.integer(max(observations$date) - min(observations$date)) - 4*7,
                               control = list(adapt_delta = 0.95, max_treedepth = 15))

# Save results to disk ----------------------------------------------------
samples_path <- here("rt-forecast-2", "deaths-conv-cases", "data", "samples", target_date)
summarised_path <- here("rt-forecast-2", "deaths-conv-cases", "data", "samples", target_date)
check_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  return(invisible(NULL))
}
check_dir(samples_path)
check_dir(summarised_path)

# save summary and samples
fwrite(forecast$samples, file.path(samples_path, "samples.csv"))
fwrite(forecast$summarised, file.path(summarised_path, "summary.csv"))

# save plots 
walk2(forecast$region, names(forecast$region), function(f, n) {
  walk(1:length(f$plots),
       ~ ggsave(filename = paste0(n, "-", names(f$plots)[.], ".png"), 
                plot = f$plots[[.]], 
                path = paste0(samples_path, "/")))
})