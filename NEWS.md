# Forecasting Covid-19 in the US

#### 2020-07-11: tidier files and formatting
- General tidying:
  - `get-us-data`: added function calls on dplyr
- `visualisation` folder:
 - Submission plotting:
   - Renamed argument for clarity: `cutoff` is now `state_min_cutoff` (for selecting states with minimum last week deaths)
   - Added back in `observed_weeks` argument to trim the plot to the last 8 weeks data
 -  Moved raw-data plotting into `out-of-date` file (untouched otherwise) as not used in main updating scripts
- `rt-forecast` folder:
  - `rt-forecast/out-of-date` has files for raw rt forecast on 2020-06-15 (which saved in a separate folder but are otherwise as expected)
  - `rt-forecast/submission-files/dated` are uniformly formatted properly (ie new formatting code run on forecasts made before the formatting clean-up: 2020-06-15 and 2020-06-22)
  - Removed downloading data for each state when running `format-rt-fn`
  - For last week's submission, the up-to-date Rt forecast is dated 2020-07-07 even though up-to-date timeseries are dated 2020-07-06
   - This is because we noticed a bug in the submitted Rt forecast, so re-saved the forecast (bug: not all quantiles of US national were included in submission file)
   - As of 2020-07-11, the "latest" Rt forecast is the 2020-07-07 version.
   - In both the "latest" and "2020-07-07", the `forecast_date` variable is set to "2020-07-06" (as this is was when the forecast was actually made)
