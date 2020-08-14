# List model names and file directories for access through the updating pipeline
#   i.e. update > ensemble > plot > score

models <- list(
  "single_models" = list(
    # Add a new model to be fed through the pipeline using the following format:
    # # Model name
    # "model_name" = list("update" = here::here(model_folder, update.R),
    #                     "submission-dated" = here::here(model_folder, output_folder, dated),
    #                     "submission-latest" = here::here(model_folder, output_folder)),
    # Rt Epinow2
    "rt_2" = list("name" = "Rt-2",
                  "update" = here::here("rt-forecast-2", "update.R"),
                  "submission-dated" = here::here("rt-forecast-2", "output", "submission-files", "dated"),
                  "submission-latest" = here::here("rt-forecast-2", "output", "submission-files")),
    # Timeseries - weekly
    "ts_weekly_deaths_only" = list("name" = "TS weekly deaths",
                         "update" = here::here("timeseries-forecast", "update.R"),
                         "submission-dated" = here::here("timeseries-forecast", "deaths-only", "submission-files", "dated"),
                         "submission-latest" = here::here("timeseries-forecast", "deaths-only", "submission-files")),
    "ts_weekly_deaths_on_cases" = list("name" = "TS weekly deaths-cases",
                             "update" = NULL,
                             "submission-dated" = here::here("timeseries-forecast", "deaths-on-cases", "submission-files", "dated"),
                             "submission-latest" = here::here("timeseries-forecast", "deaths-on-cases", "submission-files"))
    ),
  "ensemble_models" = list( 
    "mean_ensemble" = list("name" = "Mean ensemble",
                           "update" = here::here("ensembling", "quantile-average", "update-equal-quantile-average.R"),
                           "submission-dated" = here::here("ensembling", "quantile-average", "submission-files", "dated"),
                           "submission-latest" = here::here("ensembling", "quantile-average", "submission-files")),
    "qra_ensemble" = list("name" = "QRA all",
                          "update" = here::here("ensembling", "qra-ensemble", "update-qra-ensemble.R"),
                           "submission-dated" = here::here("ensembling", "qra-ensemble", "submission-files", "dated"),
                           "submission-latest" = here::here("ensembling", "qra-ensemble", "submission-files")),
    "qra_state_ensemble" = list("name" = "QRA by state",
                                "update" = here::here("ensembling", "qra-state-ensemble", "update-state-qra-ensemble.R"),
                                "submission-dated" = here::here("ensembling", "qra-state-ensemble", "submission-files", "dated"),
                                "submission-latest" = here::here("ensembling", "qra-state-ensemble", "submission-files"))
  )
)
