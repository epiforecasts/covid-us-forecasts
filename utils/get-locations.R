# Load locations
library(dplyr)

state_locations <- tigris::fips_codes %>%
  group_by(state_name) %>%
  slice_head() %>%
  select(location = state_code, state = state_name)
