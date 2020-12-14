
map_to_latest <- function(dir, date) {
  
  regions <- list.dirs(dir, recursive = FALSE)
  
  purrr::walk(regions, ~ file.copy(file.path(., date, "."),
                                   file.path(., "latest"), 
                                   recursive = TRUE))
  
}