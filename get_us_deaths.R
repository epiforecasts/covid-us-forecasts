# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  
  library(magrittr)
  
  # path_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  path_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    
  # Get & reshape data
    deaths <- readr::read_csv(path_deaths) %>% 
      dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
      tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
      dplyr::mutate(date = lubridate::mdy(date)) %>%
      dplyr::group_by(Province_State, date) %>%
      dplyr::summarise(deaths = sum(deaths)) %>%
      dplyr::rename(state = Province_State) %>%
     # De-cumulate to daily
      dplyr::arrange(date) %>% 
      dplyr::group_by(state) %>% 
      dplyr::mutate(deaths = c(0, diff(deaths)))%>%
      dplyr::mutate(deaths = replace(deaths, deaths < 0 , 0)) %>% 
      dplyr::ungroup()

  
