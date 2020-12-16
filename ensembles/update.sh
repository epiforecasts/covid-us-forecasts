#!bin/bash

# update unweighted ensembles
Rscript ensembles/models/unweighted.R

# update weighted ensembles
Rscript ensembles/models/weighted.R

# update aggregation of all models (for ensembling)
Rscript ensembles/aggregate-for-submission.R