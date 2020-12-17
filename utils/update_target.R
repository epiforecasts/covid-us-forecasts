update_target <- function(date = Sys.Date()) {
  saveRDS(as.character(date), here::here("data", "target_date.rds"))
}