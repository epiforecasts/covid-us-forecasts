#' @title Get US Death Data
#' 
#' @details
#' Get & reshape JHU data
#' Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' 
#' @param data type. Can be either "daily" or "cumulative"
#' 
#' @return data.frame with Deaths Data
#'
#' @export
#' 

get_us_deaths <- function(data = c("cumulative", "daily")){  

   # Get & reshape data
   cumulative <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
      dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
      tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
      dplyr::mutate(date = lubridate::mdy(date)) %>%
      dplyr::group_by(Province_State, date) %>%
      dplyr::summarise(deaths = sum(deaths)) %>%
      dplyr::rename(state = Province_State) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess")) %>%
      dplyr::mutate(epiweek = lubridate::epiweek(date),
            day = ordered(weekdays(as.Date(date)), 
                          levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
            epiweek = as.numeric(paste0(epiweek, ".", as.numeric(day))))
   
   saveRDS(cumulative, here::here("data", "cum_deaths_data.rds"))
   
   daily <- cumulative %>%
     # De-cumulate to daily
     dplyr::group_by(state) %>% 
     dplyr::mutate(deaths = c(0, diff(deaths)),
                   deaths = replace(deaths, deaths < 0 , 0)) %>% 
     dplyr::ungroup() 
   # Save daily deaths in all states
   saveRDS(daily, here::here("data", "deaths_data.rds"))
   
   
   if(data[1] == "cumulative"){
     return(cumulative)
   }
   if(data[1] == "daily"){  
    return(daily)
   }
}









#' @title Get US Case Data
#' 
#' @details
#' Get & reshape JHU data
#' Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
#' 
#' @param data type. Can be either "daily" or "cumulative"
#' 
#' @return data.frame with Deaths Data
#'
#' @export
#' 

get_us_cases <- function(data = c("cumulative", "daily")){
  
    # Get & reshape data
      cumulative <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                             check.names = FALSE) %>%
        dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
        tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "cases") %>%
        dplyr::mutate(date = lubridate::mdy(date)) %>%
        dplyr::group_by(Province_State, date) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::rename(state = Province_State) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess")) %>% 
        dplyr::mutate(epiweek = lubridate::epiweek(date),
                      day = ordered(weekdays(as.Date(date)), 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                      epiweek = as.numeric(paste0(epiweek, ".", as.numeric(day))))
      
      if(data[1] == "cumulative"){
        return(cumulative)
      }
      
      if(data[1] == "daily"){
        daily <- cumulative %>%
          # De-cumulate to daily
          dplyr::group_by(state) %>% 
          dplyr::mutate(cases = c(0, diff(cases)),
                        cases = replace(cases, cases < 0 , 0)) %>% 
          dplyr::ungroup() 
        # Save daily cases in all states
        saveRDS(daily, here::here("data", "cases_data.rds"))
        
        return(daily)
      }
}
    


library(magrittr)

get_us_deaths()
get_us_cases()
