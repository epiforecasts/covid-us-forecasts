# Update all models in model_list
# Edit model_list through "meta-model-list.R"

library(magrittr)

models <- readRDS(here::here("utils", "model_list.rds"))

update <- models$single_models %>%
  purrr::map( ~ here::here(file.path(.x[["root"]]), .x[["update"]])) %>% 
  unique()


safe_source <- purrr::safely(source)
purrr::walk(update, safe_source)


