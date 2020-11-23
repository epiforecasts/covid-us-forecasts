
library(magrittr); library(ggplot2); library(dplyr); library(tidyr); library(cowplot); library(data.table)

# load latest qra-ensemble
state_qra <- readr::read_csv(file = here::here("ensembling",
                                                      "qra-state-ensemble",
                                                      "submission-files",
                                                      "latest.csv"))

# sum over all locations
national_qra <- state_qra %>%
  dplyr::filter(location != "US") %>%
  dplyr::group_by(forecast_date, submission_date, target, target_end_date, type, quantile) %>%
  dplyr::summarise(value = sum(value), .groups = "drop") %>%
  dplyr::mutate(location = "US") %>%
  dplyr::bind_rows(state_qra %>%
                     dplyr::filter(location != "US")) %>%
  dplyr::select(forecast_date, submission_date, target, target_end_date, location, type, quantile, value)



# save results (dated and latest)
source(here::here("utils", "current-forecast-submission-date.R"))
data.table::fwrite(national_qra, here::here("ensembling",
                                            "qra-ensemble-sum-of-states", 
                                            "submission-files",
                                            "dated",
                                            paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv")))
data.table::fwrite(national_qra, here::here("ensembling",
                                            "qra-ensemble-sum-of-states",
                                            "submission-files",
                                            paste0("latest.csv")))


# visualise difference between national and sum-of-state forecasts
g <- national_qra %>%
  dplyr::filter(location == "US") %>%
  dplyr::mutate(method = "national forecast") %>%
  dplyr::bind_rows(state_qra %>%
                     dplyr::filter(location == "US") %>%
                     dplyr::mutate(method = "sum of state forecasts")) %>%
  dplyr::filter(grepl("inc", target),
                value < 2e4) %>%
  tidyr::pivot_wider(id_cols = c(forecast_date, submission_date, target, target_end_date, location, type, quantile), names_from = method, values_from = value) %>%
  dplyr::mutate(quantile = ifelse(is.na(quantile), 0.5, quantile)) %>%
  ggplot(aes(x = 'national forecast', y = 'sum of state forecasts', col = quantile)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  scale_color_distiller(palette = "RdBu") +
  facet_grid(. ~ target, scales = "free_x") +
  cowplot::theme_cowplot()

# save file
ggsave(filename = "submission-national-sum-of-states-compare.png",
       path = here::here("evaluation", "plots", forecast_date),
       plot = g,
       width = 16, height = 8, units = "in")


