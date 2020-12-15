#!bin/bash

# Update models
bash models/update.sh

# Update ensemble
bash ensembles/update.sh

# Update evaluation
bash evaluation/update.sh
