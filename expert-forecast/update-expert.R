library(tidyverse); library(googlesheets4); library(SHELF); library(RColorBrewer)

source("expert-forecast/get-expert.R")
source("expert-forecast/format-expert.R")
source("expert-forecast/plot-expert.R")


## Define forecast submission date (Monday)
def_date <- "2020-06-29"

## Update expert elicitation data
raw_data <- get_expert_elicitation()

## Format aggregated estimates for submission/ensembling
agg_submission_data <- format_expert_elicitation(for_forecast_date = def_date)

## Visualise results (with and without individual point estimates)
plot_expert(for_forecast_date = def_date, individual = FALSE)
plot_expert(for_forecast_date = def_date, individual = TRUE)
