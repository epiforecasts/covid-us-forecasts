# Create space by removing old files not used in code
# To keep on a branch
 
source("utils/meta-model-list.R")

models <- model_list$single_models %>%
  purrr::discard(., .p = grepl("ts_", names(.))) %>%
  names(.) %>%
  stringr::str_remove_all(., "^rt2_")

# For dated:
files <- models %>%
  purrr::map( ~ list.files(here::here(paste0("rt-forecast-2/forecast/deaths_forecast/", .x, "/state")),
                           recursive = FALSE, include.dirs = TRUE, full.names = TRUE)) %>%
  purrr::map( ~ list.files(.x, include.dirs = T, full.names = TRUE)) %>%
  purrr::flatten()

names(files) <- files
keep <- c()

files <- files %>%
  purrr::discard(., .p = grepl("(2020-10-04|2020-09-27|2020-09-20|2020-09-13)",
                               names(.))) %>%
  purrr::map(., ~ unlink(.x, recursive = TRUE))

              