
library(magrittr)
source(here::here("utils", "load-submissions-function.R"))
source(here::here("utils", "current-forecast-submission-date.R"))


# get weights ------------------------------------------------------------------

# Get past forecasts
past_forecasts <- load_submission_files(dates = "all",
                                        num_last = 5, #
                                        models = "single") 
# Remove latest week
past_forecasts <- past_forecasts[past_forecasts$forecast_date < forecast_date , ]

# Get latest forecasts
latest_forecasts <- load_submission_files(dates = "all",
                                          num_last = 1,
                                          models = "single")

# Get observed deaths
source(here::here("utils", "get-us-data.R"))
deaths_data <- get_us_deaths(data = "daily") %>%
  dplyr::group_by(epiweek, state) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last")

deaths_national <- deaths_data %>%
  dplyr::group_by(epiweek) %>%
  dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
  dplyr::mutate(state = "US")

deaths_data <- dplyr::bind_rows(deaths_data, deaths_national)

# Remove recent data
deaths_data <- deaths_data[deaths_data$epiweek < lubridate::epiweek(forecast_date) , ]

# QRA ensemble by state ---------------------------------------------------
source(here::here("ensembling", "qra-state-ensemble", "regional-qra.R"))

# List states
states <- unique(latest_forecasts$state)

# Ensemble by state
state_qra <- purrr::map(states, 
                        regional_qra,
                          past_forecasts = past_forecasts,
                          latest_forecasts = latest_forecasts,
                          deaths_data = deaths_data)

# Bind state ensembles
qra_ensemble <- dplyr::bind_rows(state_qra) %>%
  dplyr::select(-state)

# Output ------------------------------------------------------------------
# write dated file
forecast_date <- max(unique(past_forecasts$submission_date))

# Write to state-wise folder

data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-state-ensemble", 
                                            "submission-files","dated",
                                            paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# write Latest files
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-state-ensemble", "submission-files",
                                            "latest.csv"))

# Look at weights ---------------------------------------------------------
state_qra <- purrr::map(states,
                        regional_qra,
                        return_weights = TRUE,
                        past_forecasts = past_forecasts,
                        latest_forecasts = latest_forecasts,
                        deaths_data = deaths_data)

source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 5, last_week = 1) %>%
  dplyr::pull("state")

qra_ensemble <- dplyr::bind_rows(state_qra)

# save weights table
saveRDS(qra_ensemble, here::here("ensembling", "qra-state-ensemble", "weights",
                                 paste0(forecast_date, "-qra-state-weights.rds")))

# Plot
library(ggplot2)

# Get colours
source("utils/meta-model-list.R")
model_names <- unlist((model_list %>%
                         purrr::flatten() %>%
                         purrr::transpose())[["name"]])
qra_ensemble$model <- dplyr::recode(qra_ensemble$model, !!!model_names)
model_colours <- unlist((model_list %>%
                           purrr::flatten() %>%
                           purrr::transpose())[["colour"]])
names(model_colours) <- model_names

# Stacked bar
qra_ensemble %>%
  dplyr::filter(state %in% keep_states & 
                  weight > 0) %>%
  ggplot(aes(x = state, y = weight, fill = model)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = model_colours) +
  labs(x = NULL, y = "QRA state by state weight") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        legend.position = "bottom",
        text = element_text(size = 15)) +
  ggsave(filename = here::here("ensembling", "qra-state-ensemble", "weights",
                               paste0(forecast_date, "-state-weights.png")),
         height = 8, width = 20)



# summarise mean weight by model across states
weight_by_model <- qra_ensemble %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_weight = mean(weight, na.rm=T),
                   .groups = "drop") %>%
  # Plot and save
  ggplot(aes(x = model, y = mean_weight, fill = model)) +
  geom_col() +
  labs(x = NULL, y = "Mean weight in state-by-state QRA") +
  scale_fill_manual(values = model_colours) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  ggsave(filename = here::here("ensembling", "qra-state-ensemble", "weights",
                               paste0(forecast_date, "-qra-mean-weights.png")),
         width = 5)



