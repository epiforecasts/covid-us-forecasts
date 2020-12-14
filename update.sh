#!bin/bash

# Update packages
Rscript -e 'devtools::install_dev_deps()'

# Update models
bash models/update.sh

# Update ensemble
bash ensembles/update.sh

# Update evaluation
bash evaluation/update.sh

# Finalise submission
Rscript submission/finalise.R

# Submit
# See: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
