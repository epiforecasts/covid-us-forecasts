#!bin/bash

# Update target to today
Rscript -e 'source("utils/update_target.R"); update_target()'

# Update packages
Rscript -e 'devtools::install_dev_deps()'

# Update for given target
bash bin/update-for-target-date.sh

# Finalise submission
Rscript submissions/finalise.R

# Render report
Rscript -e "rmarkdown::render('submissions/report.Rmd')"

# Submit
# See: https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
