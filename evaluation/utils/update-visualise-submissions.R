library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source function for visualisation
source(here::here("evaluation", "utils", 
                  "visualise-submission-functions.R"))

source(here::here("utils", "current-forecast-submission-date.R"))

if(!dir.exists(here::here("evaluation", "plots", 
                          forecast_date))) {
  dir.create(here::here("evaluation", "plots", 
                        forecast_date))
}

plot_dir <- here::here("evaluation", "plots", forecast_date)

national_plot <- plot_forecasts(national = TRUE, 
                                obs_weeks = 8, 
                                exclude_new_epiweek = TRUE,
                                models = "single")

suppressWarnings(ggsave(filename = paste0(plot_dir, "/submission-national.png"),
       plot = national_plot, 
       width = 8, height = 8, dpi = 300))



subnational_plot <- plot_forecasts(national = FALSE, 
                                   state_min_cutoff = 5, 
                                   obs_weeks = 8,
                                   exclude_new_epiweek = TRUE,
                                   models = "single")


suppressWarnings(ggsave(filename = paste0(plot_dir, "/submission-subnational.png"), 
       plot = subnational_plot, 
       width = 24, height = 24))


# Plot ensembles only -----------------------------------------------------

national_plot <- plot_forecasts(national = TRUE, 
                                obs_weeks = 8, 
                                exclude_new_epiweek = TRUE,
                                models = "ensemble")


suppressWarnings(ggsave(filename = paste0(plot_dir, "/submission-national-ensembles.png"), 
                        plot = national_plot, 
                        width = 8, height = 8, dpi = 300))



subnational_plot <- plot_forecasts(national = FALSE, 
                                   state_min_cutoff = 5, 
                                   obs_weeks = 8,
                                   exclude_new_epiweek = TRUE,
                                   models = "ensemble")


suppressWarnings(ggsave(filename = paste0(plot_dir, "/submission-subnational-ensembles.png"), 
                        plot = subnational_plot, 
                        width = 24, height = 24))
