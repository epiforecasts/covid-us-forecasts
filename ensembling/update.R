# run mean ensemble
source(here::here("ensembling", "quantile-average", 
                  "update-equal-quantile-average.R"))

# run qra-ensemble
source(here::here("ensembling", "qra-ensemble", 
                  "update-qra-ensemble.R"))

# run crps-ensemble
source(here::here("ensembling", "crps-ensemble", 
                  "update-crps-ensemble.R"))
