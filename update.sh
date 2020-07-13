#!bin/bash

# Source this script to run a complete submission update end-to-end

# Update the data
Rscript utils/get-us-data.R

# update visualisation of the data without forecasts
Rscript evaluation/utils/update-visualise-raw-data.R

# Update Rt forecast
Rscript rt-forecast/update.R

# Update timeseries forecast
Rscript timeseries-forecast/update.R

# Update ensemble
Rscript ensembling/update.R

# Update evaluation
Rscript evaluation/update.R

# Update submission
Rscript final-submissions/update-final-submission.R

# Submit
# See: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
