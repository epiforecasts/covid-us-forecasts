library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source functions for visualisation functions
source(here::here("utils", "get-us-data.R"))

# source function for visualisation
source(here::here("visualisation", "utils", 
                  "visualise-submission-functions.R"))
source(here::here("utils", "states-min-last-week.R"))


submission_date <- Sys.Date()

if(!dir.exists(here::here("evaluation", "plots", 
                          submission_date))) {
  dir.create(here::here("evaluation", "plots", 
                        submission_date))
}


national_plot <- plot_forecasts()


ggsave(here::here("evaluation", "plots", 
                  submission_date, "submission-national.png"), 
       plot = national_plot, 
       width = 10, height = 10, dpi = 300)



subnational_plot <- plot_forecasts(national = FALSE, cutoff = 25)


ggsave(here::here("evaluation", "plots", 
                  submission_date, "submission-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 20)
