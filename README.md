# Forecasting Covid-19 in the US

This repository is set up to contribute to the [Covid-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub), which is hosted by [Reich Lab](https://reichlab.io/covid19-forecast-hub/). See the [`docs/SMG.md`](docs/SMG.md) for technical submission details.

## Models

We use an ensemble of models to forecast deaths from Covid-19 in US states. These include:
- Rt estimation: `models/rt`
- Deaths as a convolution of cases (using an Rt method to forecast cases): `models/deaths-conv-cases`
- Time-series: `models/timeseries`

These models are then ensembled using quantile regression averaging over a range of horizons and training windows. The final submission is then selected using proper scoring rules from `scoringutils`.

We [output](https://github.com/epiforecasts/covid-us-forecasts/tree/master/submissions/submitted) 1 to 4 week ahead forecasts of weekly cumulative and incident deaths, for the US and selected US states. See the [project board](https://github.com/epiforecasts/covid-us-forecasts/projects) for ongoing work.

For national and sub-national Rt estimates and forecasts in other countries, see our [website](https://epiforecasts.io/covid/posts/global/).

## Contributors

LSHTM, Centre for Mathematical Modelling of Infectious Disease:
- Sam Abbott (@seabbs)
- Sophie Meakin (@sophiemeakin)
- Kath Sherratt (@kathsherratt)
- Nikos Bosse (@nikosbosse)
- Sebastian Funk (@sbfnk) and [Epiforecasts](https://github.com/epiforecasts) team

