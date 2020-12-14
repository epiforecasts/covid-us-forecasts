#!bin/bash

# update rt deaths forecast
bash rt/update.sh

# update deaths as a convolution of cases model
bash deaths-conv-cases/update.sh

# update timeseries model
bash timeseries/update.sh

# update aggregation of all models (for ensembling)
Rscript submission/aggregate.R