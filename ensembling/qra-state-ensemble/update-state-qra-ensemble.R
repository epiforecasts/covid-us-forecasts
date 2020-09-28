
library(magrittr)
source(here::here("utils", "load-submissions-function.R"))



# get weights ------------------------------------------------------------------

# Get past forecasts
past_forecasts <- load_submission_files(dates = "all",
                                        num_last = 4, #
                                        models = "single") 
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
forecast_date <- Sys.Date()

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

# Plot and save
library(ggplot2)
source(here::here("utils", "states-min-last-week.R"))
keep_states <- states_min_last_week(min_last_week = 5, last_week = 1) %>%
  dplyr::pull("state")

qra_ensemble <- dplyr::bind_rows(state_qra)

# col plot
qra_plot <- qra_ensemble %>%
  dplyr::filter(state %in% keep_states) %>%
  ggplot(aes(x = model, y = weight)) +
  geom_col() +
  facet_wrap(.~ state) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot = qra_plot, filename = here::here("ensembling", "qra-state-ensemble", "weights",
                            paste0(Sys.Date(), "-weights.png")),
       height = 10, width = 15)

# summarise mean weight by model across states
weight_by_model <- qra_ensemble %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(`Mean weight` = mean(weight, na.rm=T),
                   .groups = "drop") %>%
  ggplot(aes(x = model, y = `Mean weight`)) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ggsave(filename = here::here("ensembling", "qra-state-ensemble", "weights",
                               paste0(forecast_date, "-qra-mean-weights.png")))

# save weights table
saveRDS(qra_ensemble, here::here("ensembling", "qra-state-ensemble", "weights",
                                paste0(forecast_date, "-qra-state-weights.rds")))

