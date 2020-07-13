library(magrittr);library(furrr)
library(dplyr);library(tidyr)
library(readr);library(stringr)
library(purrr);library(future)
library(future);library(future.apply)
library(here);library(sf); library(distill)
library(knitr);library(kableExtra); library(forecastHybrid)
library(quantgen);library(tigris); library(scoringutils)

# Drat
install.packages("drat")
library(drat)

# Quantgen
library(devtools)
install.packages("Rglpk")
install_github(repo="ryantibs/quantgen", subdir="R-package/quantgen")

# Up to date scoringutils
remotes::install_github("epiforecasts/scoringutils")
