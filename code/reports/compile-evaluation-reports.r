# packages ---------------------------------------------------------------------
library(dplyr)
library(here)
library(readr)
library(scoringutils)
library(rmarkdown)
library(covidHubUtils)
library(lubridate)
source(here("code", "config_utils", "get_hub_config.R"))

state_codes <- readRDS(here("data/state_codes.rds"))
state_codes$location_name <- state_codes$state

report_date <- readRDS(here("data/target_date.rds"))

options(knitr.duplicate.label = "allow")
suppressWarnings(dir.create(here::here("html")))


for (country in c("Overall", state_codes$location_name)) {
  rmarkdown::render(here::here("code", "reports", "evaluation",
                               "evaluation-report.Rmd"),
                    output_format = "html_document",
                    params = list(location_name = country,
                                  report_date = report_date,
                                  restrict_weeks = 4),
                    output_file =
                      here::here("html",
                                 paste0("evaluation-report-",
                                        country, ".html")),
                    envir = new.env())
}
