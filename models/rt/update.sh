#!bin/bash

# update  Rt forecast
Rscript models/rt/update-rt.R

# update submission
Rscript models/rt/update-submission.R
