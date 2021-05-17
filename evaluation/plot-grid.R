# Plot as grid by state
library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)
library(here)

# Target date -------------------------------------------------------------
target_date <- as.Date(readRDS(here("data", "target_date.rds")))
min_date <- target_date - (7*12)

# Raw data ----------------------------------------------------------------
source(here("utils", "get-us-data.R"))
source(here("evaluation", "utils", "plot_data.R"))

# Cases
cases <- get_us_data("cases", 
                     include_national = TRUE, 
                     incident_weekly = TRUE) %>%
  filter(date >= min_date & date < target_date) %>%
  mutate(value_weekly = ifelse(date == max(date), NA, value_weekly)) %>%
  split(., .$state)

plot_cases <- map(cases, ~ plot_data(.x) + 
                    labs(subtitle = "\nCases",
                         y = NULL))

# Deaths
deaths <- get_us_data("deaths", 
                   include_national = TRUE, 
                   incident_weekly = TRUE) %>%
  filter(date >= min_date & date < target_date) %>%
  mutate(value_weekly = ifelse(date == max(date), NA, value_weekly)) %>%
  split(., .$state)

plot_deaths <- map(deaths, ~ plot_data(.x) + 
                     labs(subtitle = "\nDeaths",
                          y = NULL))

# Combine by state
locations <- names(deaths)
grid_data <- map(locations, 
                      ~ plot_grid(plot_cases[[.x]], 
                                  plot_deaths[[.x]],
                                  nrow = 1,
                                  labels = .x, 
                                  hjust = 0))
names(grid_data) <- locations

# Plot all together
all_data <- plot_grid(plotlist = grid_data, 
                      ncol = 1,
                      align = "hv")
ggsave(plot = all_data,
       filename = here("evaluation", "plots", target_date,
                       "data.png"), 
       height = 150,
       width = 10,
       limitsize = FALSE)


# Forecasts ---------------------------------------------------------------
source(here("utils", "load_submissions.R"))
source(here("evaluation", "utils", "plot_forecast.R"))
source(here("utils", "summarise_submissions.R"))

obs <- get_us_data("deaths", 
                   include_national = TRUE, 
                   incident = TRUE,
                   incident_weekly = TRUE) %>%
  filter(date >= min_date & date < target_date &
         !is.na(value_weekly)) %>%
  mutate(value = value_weekly,
         value_weekly = NULL) %>%
  split(., .$state)

## Models
# Take all models, split by state, plot
models <- load_submissions(target_date, "all-models")
plotted_models <- split(models, models$state)
plotted_models <- plotted_models[names(plotted_models) %in% locations]
plot_models <- map2(plotted_models, obs, 
                    ~ plot_forecast(.x, .y) +
                      labs(subtitle = "\nModels",
                           y = NULL))
  
## Ensembles
# Take only some ensembles, split by state, plot
ensembles <- load_submissions(target_date, "ensembles", summarise = FALSE)
plotted_ensembles <- copy(ensembles)[(window == 4 & horizons == "4") | (is.na(window) & horizons == "")]
plotted_ensembles <- plotted_ensembles[model %in% c("mean", "median", "QRA")]
plotted_ensembles <- summarise_submissions(plotted_ensembles)
plotted_ensembles <- split(plotted_ensembles, plotted_ensembles$state)
plotted_ensembles <- plotted_ensembles[names(plotted_ensembles) %in% locations]
plot_ensembles <- map2(plotted_ensembles, obs, 
                       ~ plot_forecast(.x, .y) +
                         labs(subtitle = "\nEnsembles",
                              y = NULL))

# Combine by state
grid_forecasts <- map(locations, 
            ~ plot_grid(plot_models[[.x]], 
                        plot_ensembles[[.x]],
                        nrow = 1,
                        labels = .x, 
                        hjust = 0))
names(grid_forecasts) <- locations

# Plot all together
all_forecasts <- plot_grid(plotlist = grid_forecasts, 
                           ncol = 1,
                           align = "hv")
ggsave(plot = all_forecasts,
       filename = here("evaluation", "plots", target_date,
                       "forecasts.png"), 
       height = 150,
       width = 10,
       limitsize = FALSE)