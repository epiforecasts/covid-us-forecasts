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
    # Rt Epinow2 - original
    "rt2_original" = 
      list("name" = "Rt2 original",
           "root" = "rt-forecast-2",
           "update" = "update.R",
           "submission_files" = "output/original/submission-files",
           "colour" = "#FF0000"),
    # Rt Epinow2 - fixed Rt
    "rt2_fixed" = 
      list("name" = "Rt2 fixed rt",
           "root" = "rt-forecast-2",
           "update" = "update.R",
           "submission_files" = "output/fixed_rt/submission-files",
           "colour" = "#FF0099"),
    # Timeseries - weekly
    "ts_weekly_deaths_only" = 
      list("name" = "TS weekly deaths",
           "root" ="timeseries-forecast",
           "update" =  "update.R",
           "submission_files" = "deaths-only/submission-files",
           "colour" = "#336600"),
    "ts_weekly_deaths_on_cases" = 
      list("name" = "TS weekly deaths-cases",
           "root" = "timeseries-forecast",
           "update" = "update.R",
           "submission_files" = "deaths-on-cases/submission-files",
           "colour" = "#33CC00")
    # ,
    # # Expert elicitation
    # "expert" = 
    #   list("name" = "Expert",
    #        "root" = here::here("expert-forecast"),
    #        "update" = "/update-expert.R",
    #        "submission_files" = "/submission-files",
    #        "colour" = "#00FFFF")
    # Add new single models here
    ),
  # Ensembles
  "ensemble_models" = list( 
    "mean_ensemble" = 
      list("name" = "Mean ensemble",
           "root" = "ensembling", "quantile-average",
           "update" = "update-equal-quantile-average.R",
           "submission_files" = "submission-files",
           "colour" = "#66FFFF"),
    "qra_ensemble" = 
      list("name" = "QRA all",
           "root" = "ensembling", "qra-ensemble",
           "update" = "update-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6666CC"),
    "qra_state" = 
      list("name" = "QRA by state",
           "root" = "ensembling", "qra-state-ensemble",
           "update" = "update-state-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6600CC"),
    "qra_sum_states" = 
      list("name" = "QRA sum of states",
           "root" = "ensembling", "qra-ensemble-sum-of-states",
           "update" = "update-sum-of-states-qra-ensemble.R",
           "submission_files" = "submission-files",
           "colour" = "#6699FF")
    # Add new ensemble models here
  )
)

saveRDS(model_list, here::here("utils", "model_list.rds"))
