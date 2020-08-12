library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source function for visualisation
source(here::here("evaluation", "utils", 
                  "visualise-submission-functions.R"))

forecast_date <- Sys.Date()

if(!dir.exists(here::here("evaluation", "plots", 
                          forecast_date))) {
  dir.create(here::here("evaluation", "plots", 
                        forecast_date))
}


national_plot <- plot_forecasts(national = TRUE, 
                                obs_weeks = 8, 
                                exclude_new_epiweek = TRUE,
                                models = c("rt-2",
                                           "deaths-only", 
                                           "deaths-on-cases", 
                                           "mean-ensemble", 
                                           "qra-state-ensemble" #,
                                           #"qra-ensemble"
                                           ))


suppressWarnings(ggsave(here::here("evaluation", "plots", 
                                   forecast_date, "submission-national.png"), 
       plot = national_plot, 
       width = 10, height = 10, dpi = 300))



subnational_plot <- plot_forecasts(national = FALSE, 
                                   state_min_cutoff = 5, 
                                   obs_weeks = 8,
                                   exclude_new_epiweek = TRUE,
                                   models = c("rt-2",
                                              "deaths-only", 
                                              "deaths-on-cases", 
                                              "mean-ensemble", 
                                              "qra-state-ensemble" #,
                                              #"qra-ensemble"
                                              ))


suppressWarnings(ggsave(here::here("evaluation", "plots", 
                                   forecast_date, "submission-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 25))
