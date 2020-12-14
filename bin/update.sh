#!bin/bash

# Source this script to run a complete submission update end-to-end

# Update packages
Rscript -e 'devtools::install_dev_deps()'

# Update models
bash bin/update-models.sh

# Update ensemble
bash ensemble/update.sh

# Update evaluation
bash evaluation/update.sh

# Finalise submission
Rscript submission/finalise.R

# Submit
# See: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
