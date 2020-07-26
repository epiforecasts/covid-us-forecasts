# Mutate a dataframe with a "date" variable to include variables:
#   "epiweek" = lubridate::epiweek()
#   "epiweek_end" = TRUE if date is the final Saturday of an epiweek
#   "epiweek_full" = TRUE if there are a complete 7 days of data in the epiweek
#   
# Example:
# source("get_us_data.R")
# daily <- get_us_deaths()
# daily_date <- dates_to_epiweek(daily)
# 
# to only retain the last date of each epiweek (i.e. the forecasting target_end_date),
#   df %>%
#   dates_to_epiweek()
#   dplyr::filter(epiweek_end == TRUE)
# to get weekly cumulative counts from daily cumulative counts, only retain the last date of each epiweek
#   df %>%
#   dates_to_epiweek()
#   dplyr::filter(epiweek_end == TRUE)
# to get weekly incident counts from daily incidence,
#   df %>%
#   dates_to_epiweek()
#   dplyr::filter(epiweek_full == TRUE) %>%
#   dplyr::group_by(epiweek) %>%
#   dplyr::summarise(weekly_incidence = sum(daily_incidence))


dates_to_epiweek <- function(df){
  
  seq <- tibble::tibble(date = unique(df$date),
                        epiweek = lubridate::epiweek(date),
                        day = weekdays(date))
  
  epiweek_end_date <- seq %>%
    dplyr::filter(day == "Saturday")
  
  epiweek_complete <- seq %>%
    dplyr::group_by(epiweek) %>%
    dplyr::count() %>%
    dplyr::filter(n == 7) %>%
    dplyr::left_join(epiweek_end_date, by = "epiweek")
  
  df_dated <- df %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date),
                  epiweek_end = date %in% epiweek_end_date$date,
                  epiweek_full = epiweek %in% epiweek_complete$epiweek)
  
  return(df_dated)
  
}


# Epiweek to date conversion
# Takes an epiweek and returns the end date of the epiweek (i.e. target_end_date in forecasts)
# 
epiweek_to_date <- function(df){
  
  seq_dates <- tibble::tibble(
                       date = seq.Date(from = as.Date("2020-01-01"), length.out = 365, by = 1),
                       days = weekdays(date),
                       epiweek = lubridate::epiweek(date)) %>%
    dplyr::filter(epiweek %in% df$epiweek & 
                  days == "Saturday") %>%
    dplyr::select(epiweek, epiweek_end_date = date)
  
  df_dates <- df %>%
    dplyr::left_join(seq_dates, by = "epiweek")
}