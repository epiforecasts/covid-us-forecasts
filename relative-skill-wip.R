# Sequence for relative skill ensemble

library(here)

# move forecast to relevant file
source(here("utils", "move-forecast-dirs.R"))

# run evaluation
source(here("code", "reports", "compile-evaluation-reports.R"))

# run ensemble
source(here("code", "ensemble", "EuroCOVIDhub", "create-weekly-ensemble.R"))

# report ensemble
source(here("code", "reports", "compile-ensemble-report.R"))



# Join with existing plots ------------------------------------------------
library(readr)
target_date <- as.Date(readRDS(here("data", "target_date.rds")))

relskill <- read_csv(here("data-processed", "EuroCOVIDhub-ensemble", 
                          paste0(target_date, "-EuroCOVIDhub-ensemble.csv"))) %>%
  mutate(submission_date = target_date,
         model = "relative-skill-wis")

ensemble_path <- here("submissions", "ensembles", paste0(target_date, ".csv"))
ensembles <- read_csv(ensemble_path,
                      guess_max = 500000) %>%
  bind_rows(relskill)

write_csv(ensembles, ensemble_path)
