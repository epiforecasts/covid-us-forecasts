# Get a vector of states with a minimum threshold for last week deaths
# Returns a global variable
source(here::here("utils", "get-us-data.R"))
states_min_last_week <- function(min_last_week, last_week = 1){
  
daily_deaths_state <- get_us_deaths(data = "daily") %>%
  dplyr::mutate(day = ordered(weekdays(as.Date(date)), 
                              levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                epiweek_day = as.numeric(paste0(epiweek, ".", as.numeric(day))))

weekly_deaths_state <- daily_deaths_state %>%
  dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
  dplyr::group_by(state, epiweek) %>%
  dplyr::summarise(deaths = sum(deaths),
                   target_end_date = max(date),.groups = "drop_last") %>%
  dplyr::ungroup()
# over 100 cases in the last week
keep_states <- dplyr::filter(weekly_deaths_state, epiweek == max(epiweek)-last_week
                             & deaths > (min_last_week-1))

# Add codes
codes <- tigris::fips_codes %>%
                  dplyr::select(state_code, state = state_name) %>%
                  unique()

keep_states <- dplyr::left_join(keep_states, codes, by = "state")
  
return(keep_states)
}
