# run mean ensemble
source(here::here("ensembling", "quantile-average", 
                  "update-equal-quantile-average.R"))

# run qra ensemble
source(here::here("ensembling", "qra-ensemble", 
                  "update-qra-ensemble.R"))

# run qra-by-state ensemble
source(here::here("ensembling", "qra-state-ensemble", 
                  "update-state-qra-ensemble.R"))

# run crps ensemble
source(here::here("ensembling", "crps-ensemble",
                  "update-crps-ensemble.R"))


# run sum-of-states ensemble
source(here::here("ensembling", "qra-ensemble-sum-of-states",
                  "update-sum-of-states-qra-ensemble.R"))