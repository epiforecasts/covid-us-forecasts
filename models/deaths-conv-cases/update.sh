#!bin/bash

# update cases Rt forecast
Rscript models/deaths-conv-cases/update-cases.R

# update deaths based on convoluted cases
Rscript models/deaths-conv-cases/update-conv.R
