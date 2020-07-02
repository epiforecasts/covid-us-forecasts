library(tidyverse)
library(RColorBrewer)


source(here::here("visualisation", "visualise-submissions", 
                  "visualise-submission-functions.R"))


submission_date <- Sys.Date()

if(!dir.exists(here::here("visualisation", "visualise-submissions", 
                          "submission-plots", 
                          submission_date))) {
  dir.create(here::here("visualisation", "visualise-submissions", 
                        "submission-plots", 
                        submission_date))
}


national_plot <- plot_forecasts()


ggsave(here::here("visualisation", "visualise-submissions", "submission-plots", 
                  submission_date, "submission-national.png"), 
       plot = national_plot, 
       width = 10, height = 10)



subnational_plot <- plot_forecasts(national = FALSE, cutoff = 25)


ggsave(here::here("visualisation", "visualise-submissions", "submission-plots", 
                  submission_date, "submission-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 20)
