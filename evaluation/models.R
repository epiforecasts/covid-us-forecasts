# Packages ----------------------------------------------------------------
library(data.table)
library(here)
library(ggplot2)
library(purrr)
library(cowplot)

# Target date -------------------------------------------------------------
target_date <- readRDS(here("data", "target_date.rds"))

# Load observations -------------------------------------------------------
source(here("utils", "load_observations.R"))
obs <- load_observations(target_date)
# Load models -------------------------------------------------------------
source(here("utils", "load_submissions.R"))
forecasts <- load_submissions(target_date, "all-models")

# Plot forecasts ----------------------------------------------------------
source(here("evaluation", "utils", "plot_forecast.R"))

# plot usa 
us <- plot_forecast(forecasts[state == "US"], 
                    obs[state == "US" & date < target_date & date > (target_date - 7*6)])

source(here("utils", "check_dir.R"))
plot_dir <- here("evaluation", "plots", target_date, "models")
check_dir(plot_dir)
ggsave(paste0(plot_dir, "/us.png"), us, height = 7, width = 7)

# plot states
states <- plot_forecast(forecasts[state != "US"], 
                        obs[state != "US" & date < target_date & date > (target_date - 7*6)])
ggsave(paste0(plot_dir, "/states.png"), states, height = 48, width = 48)