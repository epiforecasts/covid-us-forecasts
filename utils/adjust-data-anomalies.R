# Function to adjust extreme changes in data
# - Detects % changes over 1,000% up on yesterday
# - Checks that the absolute value of this data point is over some threshold
# - If both true, sets to the NEXT day's data
# 
# source(here::here("utils", "get-us-data.R"))
# deaths_state = get_us_deaths()

# data = daily data at state level (e.g. deaths_state)
# variable = which variable to detect (e.g. cases or deaths)
# threshold = minimum absolute value of the variable, 
#            above which if an anomaly is detected, will adjust to the same as yesterday

library(dplyr)

adjust_data_anomalies <- function(data, variable_name, threshold = 100){

  names(data) <- ifelse(names(data) == variable_name, "variable", names(data))

  # Deaths data adjustment
  data <- data %>%
    group_by(state) %>%
    # Calculate % diff from previous day, only on days where data has been added
    mutate(variable = variable,
           p_diff = variable / lag(variable),
           p_diff = ifelse(p_diff == "Inf", 0, p_diff),
           extreme_diff = ifelse(p_diff > 10, TRUE, FALSE),
           adjusted = ifelse(extreme_diff == TRUE & variable > threshold, TRUE, FALSE),
           variable = ifelse(adjusted == TRUE,
                                    lag(variable),
                                    variable)) %>%
    select(-extreme_diff, -p_diff)
  
  names(data) <- ifelse(names(data) == "variable", variable_name, names(data))
  
  return(data)
  
}

         