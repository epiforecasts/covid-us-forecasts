#!bin/bash

# Score past forecasts
Rscript evaluation/score-forecasts.R

# update plots for data and forecasts
Rscript evaluation/plot-grid.R
