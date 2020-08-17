# Update all models in model_list
# Edit model_list through "meta-model-list.R"

library(magrittr)

models <- readRDS(here::here("utils", "model_list.rds"))

update <- models %>%
  purrr::flatten() %>%
  purrr::map( ~ paste0(.x[["root"]], .x[["update"]]))

for(i in update){
  source(i)
}

