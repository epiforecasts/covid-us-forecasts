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
target_date <- readRDS(here("data", "target_date.rds")) 

# Get Observations --------------------------------------------------------
# assumes no missing data (either explicit or implicit)
source(here("utils", "get-us-data.R"))
# get observed deaths
deaths_raw <- get_us_data("deaths", 
                          include_national = TRUE,
                          incident = TRUE)
deaths_raw <- as.data.table(deaths_raw)
observations <- deaths_raw[, .(region = state, date, secondary = value)]

# get observed cases
cases <- get_us_data("cases", 
                     include_national = TRUE,
                     incident = TRUE)
cases <- as.data.table(cases)
cases <- cases[, .(region = state, date = as.Date(date), 
                   primary = value)]

# join observations and filter for the last 8 weeks
observations <- merge(observations, cases, by = c("region", "date"))
observations <- observations[date <= as.Date(target_date)]
observations <- observations[date >= (max(date) - 8*7)]
setorder(observations, region, date)

# Get case forecasts ------------------------------------------------------
# load nowcast/forecast
case_forecast <- suppressWarnings(
  get_regional_results(results_dir = here("models", "deaths-conv-cases", "data", 
                                          "samples", "cases"),
                       date = ymd(target_date),
                       forecast = TRUE, samples = TRUE)$estimated_reported_cases$samples)
case_forecast <- case_forecast[date >= min(observations$date)]
case_forecast <- case_forecast[sample <= 1000]

# Forecast deaths from cases ----------------------------------------------
# set up parallel options
plan("multisession")
data.table::setDTthreads(1)

# load the prototype regional_secondary function
source_gist("https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6")

# run across Poland and Germany specifying options for estimate_secondary (EpiNow2)
forecast <- regional_secondary(observations, case_forecast,
                               return_fit = FALSE, return_plots = FALSE,
                               delays = delay_opts(list(mean = 3, mean_sd = 0.5, 
                                                        sd = 0.4, sd_sd = 0.1, max = 30)),
                               secondary = secondary_opts(type = "incidence"),
                               obs = obs_opts(scale = list(mean = 0.01, sd = 0.02)),
                               window =  3*7,
                               control = list(adapt_delta = 0.95, max_treedepth = 15))

plan("sequential")

# Save results to disk ----------------------------------------------------
samples_path <- here("models", "deaths-conv-cases", "data", "samples", "deaths", target_date)
summarised_path <- here("models", "deaths-conv-cases", "data", "summary", "deaths", target_date)

source(here("utils", "check_dir.R"))
check_dir(samples_path)
check_dir(summarised_path)

# save summary and samples
fwrite(forecast$samples, file.path(samples_path, "samples.csv"))
fwrite(forecast$summarised, file.path(summarised_path, "summary.csv"))

# save formatted forecasts 
source(here::here("utils", "format-forecast-us.R"))
formatted_forecasts <- copy(forecast$samples)
setnames(formatted_forecasts, c("value", "region"), c("deaths", "state"))
formatted_forecasts <- format_forecast_us(forecasts = formatted_forecasts,
                                          forecast_date = target_date,
                                          submission_date = target_date,
                                          shrink_per = 0)
dated_submission <- here("models", "deaths-conv-cases", "data", "submission")
check_dir(dated_submission)
fwrite(formatted_forecasts, paste0(dated_submission, "/", target_date, ".csv"))
