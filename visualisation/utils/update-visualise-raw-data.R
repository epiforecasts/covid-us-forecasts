library(tidyverse)
library(RColorBrewer)


# source functions for visualisation functions
source(here::here("utils", "get-us-data.R"))

# source function for visualisation
source(here::here("visualisation", "utils", 
                  "visualise-raw-data-functions.R"))
source(here::here("utils", "states-min-last-week.R"))


current_date <- Sys.Date()

if(!dir.exists(here::here("visualisation", "plots", 
                          current_date))) {
  dir.create(here::here("visualisation", "plots", 
                        current_date))
}


national_plot <- plot_raw_data()


ggsave(here::here("visualisation", "plots", 
                  current_date, "raw-data-national.png"), 
       plot = national_plot, 
       width = 10, height = 10)



subnational_plot <- plot_raw_data(national = FALSE, cutoff = 25)


ggsave(here::here("visualisation", "plots", 
                  current_date, "raw-data-subnational.png"), 
       plot = subnational_plot, 
       width = 20, height = 20)
