library(ggplot2)
library(RColorBrewer)
library(dplyr)


# source function for visualisation
source(here::here("evaluation", "utils", 
                  "visualise-raw-data-functions.R"))


source(here::here("utils", "current-forecast-submission-date.R"))

if(!dir.exists(here::here("evaluation", "plots", 
                          forecast_date))) {
  dir.create(here::here("evaluation", "plots", 
                        forecast_date))
}


national_plot <- plot_raw_data(national = TRUE, obs_weeks = 8, exclude_new_epiweek = TRUE)


suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                                            forecast_date, "raw-data-national.png"), 
       plot = national_plot, 
       width = 10, height = 10))



subnational_plot <- plot_raw_data(national = FALSE, state_min_cutoff = 5, obs_weeks = 8,
                                  exclude_new_epiweek = TRUE)


suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots", 
                  forecast_date, "raw-data-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 20))
