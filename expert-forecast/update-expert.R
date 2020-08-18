# Download and format expert elicitation forecasts for given forecast date
library(tidyverse); library(googlesheets4); library(SHELF); library(RColorBrewer)

# Set up functions
source(here::here("utils", "get-us-data.R"))
source(here::here("utils", "dates-to-epiweek.R"))

source("expert-forecast/get-expert.R")
source("expert-forecast/format-expert.R")
source("expert-forecast/plot-expert.R")


# Define forecast submission date (a Monday)
def_date <- lubridate::floor_date(Sys.Date(), unit = "week", week_start = 1)


# Download most recent expert elicitation forecasts
get_expert_elicitation()


# Format (mean aggregated) forecasts for submission/ensembling
format_expert_elicitation(for_forecast_date = def_date,
                          submission_quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))


# Can visualise expert forecasts with plot_expert()
plot_expert(for_forecast_date = def_date, individual = FALSE)
# plot_expert(for_forecast_date = def_date, individual = TRUE)
