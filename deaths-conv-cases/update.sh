#!bin/bash

# update cases Rt forecast
Rscript deaths-conv-cases/update-cases.R

# update deaths based on convoluted cases
Rscript deaths-conv-cases/update-conv.R
