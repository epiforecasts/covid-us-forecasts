# Forecasting Covid-19 in the US

This repo is set up to contribute to the [Covid-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub), which is hosted by [Reich Lab](https://reichlab.io/covid19-forecast-hub/).

## Methods
We use an ensemble of models to forecast deaths from Covid-19 in US states. These include:
- Rt estimation: [code](https://github.com/epiforecasts/covid-us-forecasts/tree/master/rt-forecast-2), relying on methods developed in [EpiNow2](https://epiforecasts.io/EpiNow2/)
- Time-series: [code](https://github.com/epiforecasts/covid-us-forecasts/tree/master/timeseries-forecast), including both pure autoregressive time-series, and with case predictors (current cases and leading cases by 1 and 2 weeks)

We [output](https://github.com/epiforecasts/covid-us-forecasts/tree/master/final-submissions/death-forecast) 1 to 4 week ahead forecasts of weekly cumulative and incident deaths, for the US and selected US states. See the [project board](https://github.com/epiforecasts/covid-us-forecasts/projects) for ongoing work.

For national and sub-national Rt estimates and forecasts in other countries, see our [website](https://epiforecasts.io/covid/posts/global/).

## Development
Please add an issue if you would like to get in touch or contribute.
##### Contributors
LSHTM, Centre for Mathematical Modelling of Infectious Disease:
- Sam Abbott (@seabbs)
- Sophie Meakin (@sophiemeakin)
- Kath Sherratt (@kathsherratt)
- Nikos Bosse (@nikosbosse)
- Sebastian Funk (@sbfnk) and [Epiforecasts](https://github.com/epiforecasts) team

## Technical notes
> Note: for Epiforecasts team using the met server:
> 1. Check the server is available for the next couple of hours
> 2. Log in with team's credentials
> 3. Check with the last user and delete any existing files on the server. Except for the `kinematic` folder, the storage and environment should be empty
> 4. Open a new R project and clone this repository from Github
> - Caution - when interacting with Github, do not use the RStudio git GUI, as this crashes the server

> Using Azure:
> - Log in to Azure
> - Start and connect to kath-test using ssh
> - Use R Studio to open a new project, clone this repository

### Create fresh forecasts
For an end-to-end run (including new Rt and timeseries models, mean and QRA ensembles, formatted ready for submission):
- Run `system("update.sh")`
- Check the output by looking plots saved in the most recent folder: `evaluation/plots/yyyy-mm-dd`
- A properly formatted csv, ready to be submitted to the Reich forecast hub, will be saved in 
  - `final-submissions/death-forecast/yyyy-mm-dd-epiforecasts-ensemble1.csv`

Note that the final submission file can be either the quantile mean average or the QRA ensemble. To check or change this:
  - Open `final-submissions/update-final-submission.R`
  - Set global variables:
   - `ensemble_dir <- "qra-ensemble" # or "quantile-average"`
   - `ensemble <- "qra" # or "qa`
  - If doing this:
    - _Before running_ `update.sh`: save the script and then run `update.sh`
    - _After running_ `update.sh`: save and then source the script (and the submission-ready csv will be re-written)

### Runtime
On the Epiforecasts met server (48 core, 64gb), if `update.sh` runs perfectly then the runtime is typically around an hour and a half. This comes from the Rt forecast, which takes ~1h 20 min to run. If needed, all other scripts can be run on a standard laptop in a few minutes.

### Debugging
- If `update.sh` errors: open  as a script, run each command in sequence to check where it fails
- Check dependencies are up to date (some ideas for this are in `utils/package-check.R`)
- If ensembling is failing:
  - I have had trouble installing the `quantgen` package on the met server. However I was able to install the package on my laptop.
  - If you run into the same problem and can install `quantgen` locally:
    - Run the first 4 update commands in `update.sh` (data and forecasts) on the server
    - Push all changes (i.e. the new forecasts) to github
    - Clone locally and install `quantgen`
    - Then run the remaining 3 update commands (ensembling, evaluation and final submission) - this only takes a few minutes on a standard laptop
    - Push to github, submit csv to Forecast Hub
