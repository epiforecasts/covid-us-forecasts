library(dplyr)


deaths_on_cases <- format_timeseries_forecast(model_type = "deaths-on-cases")
readr::write_csv(deaths_on_cases, here::here("timeseries-forecast", "deaths-on-cases",
                                             "submission-files",
                                             "latest-weekly-deaths-on-cases.csv"))

deaths_only <- format_timeseries_forecast(model_type = "deaths-only")
readr::write_csv(deaths_only, here::here("timeseries-forecast", "deaths-only",
                                         "submission-files",
                                         "latest-weekly-deaths-only.csv"))



