# Update all models in model_list
# Edit model_list through "meta-model-list.R"

library(magrittr)

models <- readRDS(here::here("utils", "model_list.rds"))

update <- models %>%
  purrr::flatten() %>%
  purrr::map( ~ here::here(.x[["root"]], .x[["update"]])) %>% 
  unique()

purrr::walk(update, source)


