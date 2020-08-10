
library(magrittr)
source(here::here("utils", "load-submissions-function.R"))



# get weights ------------------------------------------------------------------

# Get past forecasts
past_forecasts <- load_submission_files(dates = "all",
                                        num_last = 4, #
                                        models = c("rt-2",  "deaths-only", "deaths-on-cases")) 
# Get latest forecasts
latest_forecasts <- load_submission_files(dates = "latest",
                                          models = c("rt-2", "deaths-only", "deaths-on-cases"))

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
source(here::here("ensembling", "qra-ensemble", "regional-qra.R"))

# List states
states <- as.list(unique(latest_forecasts$state))

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

# data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", 
#                                             "submission-files","dated",
#                                     paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# # write Latest files
# data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", "submission-files",
#                                             paste0("latest-epiforecasts-ensemble1-qra.csv")))


# Write to state-wise folder

data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-state-ensemble", 
                                            "submission-files","dated",
                                            paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
# write Latest files
data.table::fwrite(qra_ensemble, here::here("ensembling", "qra-ensemble", "submission-files",
                                            paste0("latest-epiforecasts-ensemble1-qra.csv")))
