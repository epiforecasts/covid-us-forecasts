# Packages ----------------------------------------------------------------
library(here)
library(data.table)

# Target date -------------------------------------------------------------
target_date <- readRDS(here("data", "target_date.rds"))

# Choose submission -------------------------------------------------------
submission <- fread(here("submissions", "ensembles", paste0(target_date, ".csv")))
submission <- submission[model == "mean"]

# Convert -----------------------------------------------------------------
submission <- submission[, model := NULL]
submission <- submission[, submission_date := NULL]

# Check  ------------------------------------------------------------------

# Save submission ---------------------------------------------------------
fwrite(submission, here("submissions", "submitted",
                        paste0(target_date, "-epiforecasts-ensemble1.csv")))
