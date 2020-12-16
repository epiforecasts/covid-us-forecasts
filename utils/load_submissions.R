library(data.table)
library(here)

load_submissions <- function(target_date, folder = "all-models", summarise = TRUE,
                             horizons, window) {
  forecasts <- fread(here("submissions", folder, paste0(target_date, ".csv")))
  
  if (summarise) {
    source(here("utils", "summarise_submissions.R"))
    forecasts <- summarise_submissions(forecasts)
  }
  return(forecasts)
}