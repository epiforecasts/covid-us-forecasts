library(tidyverse)
library(RColorBrewer)


source(here::here("visualisation", "visualise-raw-data", 
                  "visualise-raw-data-functions.R"))


current_date <- Sys.Date()

if(!dir.exists(here::here("visualisation", "visualise-raw-data", 
                          "raw-data-plots", 
                          current_date))) {
  dir.create(here::here("visualisation", "visualise-raw-data", 
                        "raw-data-plots", 
                        current_date))
}


national_plot <- plot_raw_data()


ggsave(here::here("visualisation", "visualise-raw-data", "raw-data-plots", 
                  current_date, "raw-data-national.png"), 
       plot = national_plot, 
       width = 10, height = 10)



subnational_plot <- plot_raw_data(national = FALSE, cutoff = 25)


ggsave(here::here("visualisation", "visualise-raw-data", "raw-data-plots", 
                  current_date, "raw-data-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 20)
