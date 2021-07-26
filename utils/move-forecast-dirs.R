# move from current directories to data-processed/
library(here)
library(purrr)
library(readr)

# Re-save all models

# models
models <- dir(here("models"))
models <- models[!grepl("\\.", models)]

forecasts <- map(models, 
                 ~ dir(here("models", .x, "data", "submission"),
                       full.names = TRUE))
old <- unlist(forecasts)

set_new_path <- function(old_path) {
  # get file path
  old_path <- gsub("C:/Users/kaths/GitHub/covid-us-forecasts/", "", old_path)
  new_path <- gsub("models", "data-processed", old_path)
  new_path <- gsub("data/submission/", "", new_path)
  
  # get model name
  model_name <- gsub("data-processed/", "", new_path)
  model_name <- gsub("/....-..-..\\.csv", "", model_name)
  
  # set new path
  new_path <- gsub("\\.csv", paste0("-", model_name, ".csv"), new_path)
  new_path <- paste0(here(), "/", new_path)
  return(new_path)
}

new <- map(old, set_new_path)
new <- unlist(new)

# create dirs
walk(models, 
     ~ dir.create(here("data-processed", .x)))

# read-write
walk2(.x = old, .y = new,
     ~ read_csv(.x) %>%
       write_csv(., file = .y))

