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
> Note: for Epiforecasts team using azure
> - Log in to Azure portal and start [epiforecasts-general-purpose VM](https://portal.azure.com/#@epiforecastsoutlook.onmicrosoft.com/resource/subscriptions/2d9a656e-d2ff-4b55-9f35-99bddf874f1b/resourceGroups/epiforecasts-general-purpose/providers/Microsoft.Compute/virtualMachines/epiforecasts-general-purpose/overview)
> - Connect to the azure VM over ssh, with IP 13.92.183.137 and given username and password
> - Run: `sudo docker start epinow2`
> - Open a local internet browser, navigate to http://13.92.183.137:8787/, and login with epinow2
> - Use R Studio (checkout this repository, create forecasts as below)
> - When finished, reopen the ssh terminal (logged in to azure VM) and run `sudo docker stop epinow2`
> - Back in Azure portal, stop the VM

### Create fresh forecasts
For an end-to-end run (including new Rt and timeseries models, mean and QRA ensembles, formatted ready for submission):
- Run `update.sh` from the terminal
- Check the output by looking plots saved in the most recent folder: `evaluation/plots/yyyy-mm-dd
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
Using the Epiforecasts azure VM (72 core), if `update.sh` runs perfectly then the runtime is typically 1-2 hours

### Debugging
- If `update.sh` errors: open  as a script, run each command in sequence to check where it fails
- Check dependencies are up to date (some ideas for this are in `utils/package-check.R`)
