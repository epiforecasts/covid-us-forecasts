update_target <- function(date = Sys.Date()) {
  saveRDS(date, here::here("data", "target_date.rds"))
}