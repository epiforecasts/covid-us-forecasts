# Checking all dependencies ----------------
devtools::install_deps()

library(EpiNow);library(EpiSoon)
library(magrittr);library(furrr)
library(dplyr);library(tidyr)
library(readr);library(stringr)
library(purrr);library(future)
library(future);library(future.apply)
library(here); library(forecastHybrid)
library(quantgen);library(tigris); library(scoringutils)

# For Rt estimates -------------------------
# Check that EpiNow / EpiSoon and their deps are up to date
# install.packages("drat")
# library(drat)
# drat::addRepo("epiforecasts")
# install.packages("EpiSoon")
# library(devtools)
# devtools::install_deps(repos = "https://epiforecasts.io/drat/")


# For ensembling ----------------------------
# Quantgen
# library(devtools)
# install.packages("Rglpk")
# install_github(repo="ryantibs/quantgen", subdir="R-package/quantgen")

# Up to date scoringutils
# remotes::install_github("epiforecasts/scoringutils")
