# covid-us-forecasts evaluation: execution handoff

Self-contained brief for Claude. Everything needed to execute steps 3 and 4 is here; no prior conversation required. Steps 1 and 2 are parked and summarised at the end so they are not lost.

## What the project is

The repo `epiforecasts/covid-us-forecasts` produced weekly quantile forecasts of US COVID-19 deaths (1 to 4 week ahead, incident and cumulative) for 50 states, DC and the US national, and submitted one chosen forecast each week to the Reich Lab COVID-19 Forecast Hub. The work runs from roughly May 2020 to March 2022.

Six models, three base and three ensemble:

| Label in data | Type | Source |
|---|---|---|
| `Rt` | base | EpiNow2 renewal model fit directly to deaths (`models/rt`) |
| `Case convolution` | base | EpiNow2 on cases, then convolved to deaths (`models/deaths-conv-cases`) |
| `Timeseries` | base | ARIMA + ETS via EpiSoon/forecastHybrid, deaths regressed on cases (`models/timeseries`) |
| `mean` | ensemble | per-quantile mean of the three base models (`ensembles/models/unweighted.R`) |
| `median` | ensemble | per-quantile median of the three base models |
| `QRA` | ensemble | quantile regression averaging via quantgen, default window 4 / horizon 4 (`ensembles/models/weighted.R`) |

## The four-step design

1. Model quality: rebuild the six models with modern packages (variant M). PARKED.
2. Implementation quality: rebuild with era packages but implemented correctly (variant P), contrasted with the original real-time forecasts (variant O). PARKED.
3. Predictive quality: score the existing forecasts. ACTIVE, this brief.
4. Model selection against human judgement: human-in-the-loop versus automated. ACTIVE, this brief.

Steps 3 and 4 run on the original forecasts already in the repo, so no model refitting is needed now.

## Locked decisions

- Scoring outcome (truth): Reich Lab Hub `truth-Incident Deaths.csv`, March 2022 vintage. Pull it from `reichlab/covid19-forecast-hub` git history at an end-of-March-2022 commit. Cross-check with epidatr `as_of` March 2022 if convenient.
- Automated comparator for step 4: the QRA alone (already skill-weighted), with the median ensemble only as the documented fallback where QRA is absent.
- US national: carried through but reported and analysed separately from states, never pooled.
- Scales: natural, per-100k, and log (`log_shift`, offset 1).
- scoringutils version 2.x API.
- Pairwise relative-skill baseline: median ensemble (proposed; confirm with Kath before finalising step 4).

## Repository facts

- The full clone is about 1.5 GB because of forecast history. For steps 3 and 4, do a shallow sparse checkout of only: `ensembles/data`, `submissions/all-models`, `submissions/submitted`, `submissions/submission-notes`, `data`, and `utils`.
- Forecast files are Hub long format: `forecast_date, submission_date, target, target_end_date, location, type, quantile, value, model`. `target` looks like `1 wk ahead inc death`; `location` is FIPS (US national is `US`); 23 quantiles plus a `point` row.
- Base-model forecasts: `submissions/all-models/{date}-all-models.csv`. Ensembles: `ensembles/data/unweighted/{date}.csv` (mean and median) and `ensembles/data/weighted/forecasts/{date}.csv` (QRA), with QRA weights in `ensembles/data/weighted/weights/`.
- The submitted (human-in-the-loop) forecast each week: `submissions/submitted/{date}-epiforecasts-ensemble1.csv`.
- The human override records: `submissions/submission-notes/{date}-swap-ensemble.rds`, 37 weeks with at least one override. `finalise.R` and `swap-ensembles.R` show the logic: default is QRA window 4 / horizon 4, median fallback, with specific states swapped to another model after visual review of the weekly `report.Rmd`.
- Crosswalks: `data/state_pop.csv` (`state_code, tot_pop, state_name`) and `data/state_codes.rds` (FIPS to name).

## Setup for Cowork

1. Sparse, shallow checkout of the paths listed above.
2. R with: tidyverse, scoringutils (>= 2.x), lubridate, here, and lme4 (step 4 regression). Use renv to lock. quantgen is not needed for steps 3 to 4.
3. Quarto for rendering the two documents.
4. Build the cached scored object first (below); both documents read it.

## Shared build: `R/build_scores.R` (run once, cache)

```r
# R/build_scores.R  -- build the scored object once, cache it
library(tidyverse)   # native pipe throughout
library(scoringutils)

# 1. Load all six models' inc-death quantile forecasts into one long tibble:
#    base models from submissions/all-models/, ensembles from ensembles/data/.
#    Expected columns: forecast_date, target, target_end_date, location,
#    type, quantile, value, model.

# 2. Load truth: Reich hub weekly incident deaths, March 2022 vintage.
#    Columns wanted: location (FIPS), target_end_date, observed.

# 3. Load population crosswalk: data/state_pop.csv joined to FIPS.

fc <- forecasts |>
  filter(type == "quantile") |>                          # drop point rows
  mutate(horizon = parse_number(target)) |>              # 1-4 wk from target string
  left_join(truth, by = c("location", "target_end_date")) |>
  left_join(pop,   by = "location") |>
  mutate(geo = if_else(location == "US", "national", "state")) |>  # split US out everywhere
  rename(predicted = value, quantile_level = quantile) |>
  as_forecast_quantile(
    forecast_unit = c("model", "forecast_date",
                      "target_end_date", "location", "horizon", "geo")
  )

# per-100k: scale predicted and observed before scoring
fc_pc <- fc |>
  mutate(predicted = predicted / (tot_pop / 1e5),
         observed  = observed  / (tot_pop / 1e5))

# log: transforms predicted and observed, keeps natural too (append = TRUE)
fc_log <- transform_forecasts(fc, fun = log_shift, offset = 1, append = TRUE)

scores <- bind_rows(
  score(fc)     |> mutate(scale = "natural"),
  score(fc_pc)  |> mutate(scale = "per_100k"),
  score(fc_log) |> mutate(scale = "log")
)
saveRDS(scores, "data/scores.rds")
```

## Step 3: scoringutils vignette (`vignette-scoring.qmd`)

Package-style article, six chunks. Reads `data/scores.rds`.

1. Observed data and its distribution. Weekly incident deaths over time, and the outcome distribution across targets, raw and per-100k, states separate from US. Purpose: motivate the scale choices via the right skew and cross-state spread.
2. Score the six models, natural scale. `summarise_scores(by = c("model", "geo"))`: WIS with overprediction, underprediction and dispersion components, ae_median, bias, 50/90 percent coverage. One table, one decomposition figure.
3. Scoring choices: per-capita and log. Compare model ranks across the three scales already in the cache. Rank table plus a short read; cite Bosse et al. 2023 for the log rationale.
4. Mean ensemble, absolute error. Filter `model == "mean"`; ae_median overall, then by horizon and by location, US separate. Horizon trend plus a state map or ranked bar.
5. Metrics for different user needs, five sub-chunks: catalogue the components and the question each answers; map to three stylised users (planner fearing overprediction, surge planner fearing upper-tail under-coverage, methodologist wanting one proper score plus calibration), as examples not recommendations; rank instability across metrics (bump chart); metric redundancy via `plot_correlations()`; and reuse chunk 3 to show scale choice reorders models as much as metric choice.
6. Closing hook. Placeholder noting the same object will later carry the O, P and M variants for the implementation and science decomposition. No analysis yet.

## Step 4: human judgement versus automated selection

Two documents. Write and timestamp `prereg.qmd` before computing any step-4 scores; then `analysis.qmd` executes it.

Pre-registration contents:

1. Background and estimand. Difference in skill (relative WIS, log scale) between the submitted human-in-the-loop forecast and the automated QRA, on the matched location-weeks where an override occurred (primary) and over all location-weeks (secondary), US national reported separately.
2. Forecast streams. Automated default = QRA window 4 / horizon 4, median fallback. Human-in-the-loop = default plus recorded swaps (equals `submissions/submitted/`). Oracle = best per location-week with hindsight, labelled upper bound only.
3. Reconstruction and validation. Rebuild the automated default from `ensembles/data`; apply each week's overrides from `submission-notes/`; confirm the rebuilt stream reproduces the submitted CSVs exactly; record mismatches.

```r
# Reconstruct the two streams, validate against what was submitted
swaps <- list.files("submissions/submission-notes",
                    pattern = "swap-ensemble", full.names = TRUE) |>
  set_names() |>
  map(readRDS) |>
  bind_rows(.id = "file")    # which states overridden, to which model, each week

automated <- qra_default     # QRA w4/h4, median fallback
hitl <- automated |>
  rows_update(swaps_as_forecasts,
              by = c("location", "forecast_date", "target"))   # apply overrides
# check hitl == submitted per location-week; flag any mismatch
```

4. Outcomes. Primary: relative WIS, log scale, via pairwise comparisons against the median-ensemble baseline so missing forecasts do not bias the comparison. Secondary: WIS components, ae_median, 50/90 percent coverage. Descriptive: which model was actually submitted, displayed over time.
5. Stratification, pre-specified: horizon; epidemic phase (growth, peak, decline, low); difficulty tier (recent observed volatility); override versus not.
6. Analyses. Paired automated-versus-human comparison on the matched set. Ensemble-versus-base regression:

```r
library(lme4)
# log-scale relative WIS, ensemble vs base, random effects for place and week
fit <- lmer(log_rel_wis ~ is_ensemble + horizon +
              (1 | location) + (1 | forecast_date),
            data = model_scores)
```

The representative "typical" target, defined now to avoid cherry-picking, as the location-week whose per-model relative WIS is closest to that model's median, shown as a single forecast.

7. Bias and confounding. Look-ahead (operational streams use only information available at the forecast date; oracle is the labelled exception); confounding by difficulty (condition via stratification and the mixed model); missingness (pairwise relative skill, restrict to common coverage where needed); multiplicity (one primary estimand, correction pre-specified for the rest); outcome skew (log and relative scales, rank-based sensitivity); reconstruction error (the validation above).
8. Confirmatory versus exploratory. Confirmatory: the primary paired comparison and the ensemble regression. Exploratory: phase and difficulty interactions, per-state patterns, the representative-target display.
9. Sensitivity analyses. Natural and per-capita scales; include versus exclude US national; truth vintage.
10. Extension (parked under step 4). Whether overrides that helped the standalone submission also helped the downstream Reich Hub ensemble (needs Hub ensemble files); and re-running the human-versus-automated question with the modern M base models once steps 1 to 2 are built.

## Build order

`R/build_scores.R` first; then `prereg.qmd` (timestamp before scoring); then `vignette-scoring.qmd`; then `analysis.qmd`.

## Open micro-decision

Confirm the pairwise baseline (median ensemble proposed). Everything else is locked.

## Parked: steps 1 and 2 (resume later, do not start now)

- Variant O = original real-time forecasts (already in the repo). Variant P = perfect rebuild with era packages. Variant M = modern rebuild. Contrasts: O vs P isolates real-time implementation error; P vs M isolates package and method improvement.
- Real-time model inputs reconstructed via epidatr `as_of` per forecast date (Delphi JHU-CSSE state signals). Scoring outcome stays the March 2022 Reich truth.
- Run on a 10% stratified (epidemic phase, calendar time, location) subset of location-weeks; one regional fit per location-week yields all four horizons.
- As-of codebase: the project tracked the EpiNow2 dev branch. Major release timeline: pre-CRAN dev to October 2020; first CRAN interface release 18 October 2020 (breaking changes, NUTS or VI); 1.3.1 late November 2020 (functional rewrite, truncation, observed-data scaling, population-susceptibility adjustment, obs_opts/rt_opts interface); 1.3.2 13 December 2020 (experimental estimate_secondary added); long dev period through 2021 to early 2022 covering most project weeks; 1.3.3 20 October 2022, after the project ended. Pin P to a few representative dev snapshots aligned to these boundaries and read the as-of model code from git history, not current master.
- Modern rebuild (M): replace the removed `get_generation_time(source="ganyani")` and `get_incubation_period(source="lauer")` helpers with explicit `Gamma()`/`LogNormal()` distributions (or epiparameter) inside `gt_opts()` and `delay_opts()`; LogNormal Rt prior; `data =` argument; packaged `estimate_secondary()`/`forecast_secondary()` instead of the live gist; rebuild the timeseries model in fable (EpiSoon is archived, forecastHybrid stale).
- Delineation: P fixes user-code errors (live gist dependency, the wrong-region "Poland and Germany" comment, silent regional fit failures, unstandardised priors) but reproduces era package behaviour including era package bugs; fixing package-level limitations belongs to M. Account for MCMC run-to-run noise with fixed seeds and a few repeated fits.

## References

- scoringutils v2.x functions and workflow: epiforecasts.io/scoringutils.
- Log-scale scoring rationale: Bosse et al. 2023, PLoS Computational Biology 19(8): e1011393.
- Weighted interval score: Bracher et al. 2021.
- US Hub ensemble context: Cramer et al. 2022, PNAS.
- Repo logic: `finalise.R`, `swap-ensembles.R`, `ensembles/models/*.R`, `report.Rmd`.
