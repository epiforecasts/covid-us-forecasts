library(data.table)
library(here)

load_observations <- function(target_date) {
  obs <- fread(here("models", "rt", "data", "summary", target_date, "reported_cases.csv"))
  obs <- obs[, .(date, state = region, value = confirm)]
  
  state_codes <- readRDS(here("data", "state_codes.rds"))
  obs <- obs[state_codes, on = "state"]
  
  source(here("utils", "dates-to-epiweek.R"))
  obs <- dates_to_epiweek(obs)
  obs <- obs[epiweek_full == TRUE, .(value = sum(value), date = max(date)), 
             by = .(location, state, epiweek)][, epiweek := NULL]
  return(obs)
}