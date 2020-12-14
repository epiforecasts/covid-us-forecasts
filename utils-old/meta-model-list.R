# List model names and file directories for access through the updating pipeline
#   i.e. update > ensemble > plot > score

model_list <- list(
  # Models
  "single_models" = list(
    # --------------
    # Add a new model (single or ensemble) to be fed through the pipeline using the following format:
    # model_name = 
    # list("name" = "model_name",                                       # Unique model name e.g. used in plotting
    #      "root" = here::here(model_folder),                           # Root model folder - may be shared with other models
    #      "update" = "/update.R",                                      # Update script accessed from root  - may be shared
    #      "submission_file" = "/output/model_name/submission-files",   # Model specific output files
    #      "colour" = "colour_name")                                    # Plotting colour: try https://htmlcolorcodes.com/color-chart/
    # --------------
    # 
    # Rt Epinow2 - fixed future Rt
    "rt2_fixed_future_rt" =
      list("name" = "Rt2 fixed future rt",
           "root" = "rt-forecast-2",
           "update" = "update.R",
           "submission_files" = "output/fixed_future_rt/submission-files",
           "colour" = "#DB7093"),
    "secondary" =
      list("name" = "secondary",
           "root" = "deaths-conv-cases",
           "update" = "update.R",
           "submission_files" = "data/submission",
           "colour" = "#326194"),
    "ts_weekly_deaths_on_cases" = 
      list("name" = "TS weekly deaths-cases",
           "root" = "timeseries-forecast",
           "update" = "update.R",
           "submission_files" = "deaths-on-cases/submission-files",
           "colour" = "#33CC00")
    ),
  # Ensembles
  "ensemble_models" = list( 
    "mean_ensemble" = 
      list("name" = "Mean ensemble",
           "root" = "ensembling/quantile-average",
           "update" = "update-equal-quantile-average.R",
           "submission_files" = "submission-files",
           "colour" = "#66FFFF"),
    "qra_ensemble" = 
      list("name" = "QRA all",
           "root" = "ensembling/qra-ensemble",
           "update" = "update-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6666CC"),
    "qra_state" = 
      list("name" = "QRA by state",
           "root" = "ensembling/qra-state-ensemble",
           "update" = "update-state-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6600CC"),
    "qra_sum_states" = 
      list("name" = "QRA sum of states",
           "root" = "ensembling/qra-ensemble-sum-of-states",
           "update" = "update-sum-of-states-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6699FF")
    # Add new ensemble models here
  )
)
saveRDS(model_list, here::here("data", "model_list.rds"))
