# System Maintenance Guide 

## Create fresh forecasts manually

For an end-to-end run:

- Check your local version of the repository is up to date with `git pull`
- Run `bash/forecast.sh` from the terminal
- Check the output by looking plots saved in the most recent folder: `evaluation/plots/yyyy-mm-dd`
- A properly formatted csv, ready to be submitted to the Reich forecast hub, will be saved in 
  - `submissions/submitted/yyyy-mm-dd-epiforecasts-ensemble1.csv`

Note that the final submission file is a QRA ensemble with weighted quantiles by default. To check or change this:
  - Open `submissions/finalise.R`
  - Set global variables:
    - alter line 12 to filter for a different model.
  - If doing this:
    - _Before running_ `bin/forecast.sh`: save the script and then run `bin/forecast.sh`
    - _After running_ `bin/forecast.sh`: save and then source the script.

## Runtime

Using the Epiforecasts azure VM (16 core), if `bash/forecast.sh` runs perfectly then the runtime is typically 1-2 hours

## Debugging

- If `bin/forecast.sh` errors: open  as a script, run each command in sequence to check where it fails
