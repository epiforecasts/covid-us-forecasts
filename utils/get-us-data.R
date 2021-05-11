# Get & reshape JHU data for deaths or cases
# 
# Args:
# include_national FALSE whether to include US as the sum of states
# incident_daily TRUE returns incident, FALSE returns cumulative
# incident_weekly FALSE whether to add a column as weekly sum of daily incidence
#
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
  
get_us_data <- function(data = c("deaths", "cases"),
                        include_national = FALSE,
                        incident = TRUE, 
                        incident_weekly = FALSE) {  
  
  if (data == "cases") {
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  } else if (data == "deaths") {
    url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  }

   # Get data & reshape from wide to long by date
   cumulative <- suppressMessages(read_csv(url)) %>% 
      select(Province_State, matches("^\\d")) %>%
      pivot_longer(cols = -Province_State, names_to = "date", values_to = "value") %>%
      mutate(date = mdy(date)) %>%
      group_by(Province_State, date) %>%
      summarise(value = sum(value), .groups = "drop_last") %>%
      rename(state = Province_State) %>%
      arrange(date) %>%
      filter(!state %in% c("Diamond Princess", "Grand Princess"))

   # Arg: include US national sum of states as a "state"
   if (include_national) {
     national <- cumulative %>%
       group_by(date) %>%
       summarise(value = sum(value), .groups = "drop") %>%
       mutate(state = "US")
     cumulative <- bind_rows(cumulative, national)
   }
   
   # Add epi dates
   cumulative <- cumulative %>%
     mutate(epiweek = epiweek(date),
                   epiyear = epiyear(date))
   
   # Arg: return cumulative
   if (!incident) {
     return(cumulative)
   }
   
   daily <- cumulative %>%
     group_by(state) %>% 
     mutate(value = c(0, diff(value)),
                   value = replace(value, value < 0 , 0)) %>%
     ungroup()
     
   # Arg: add weekly incidence column
   if (incident_weekly) {
     weekly <- daily %>%
       group_by(state, epiyear, epiweek) %>%
       summarise(value_weekly = sum(value), 
                 date = max(date),
                 .groups = "drop") %>%
       select(state, date, value_weekly)
     
     daily <- left_join(daily, weekly, by = c("state", "date"))
   }

   return(daily)
}

