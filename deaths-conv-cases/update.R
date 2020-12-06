library(here)
# update cases Rt forecast
source(here("deaths-conv-cases", "update-cases.R"))

# update deaths based on convoluted cases
source(here("deaths-conv-cases", "update-conv.R"))
