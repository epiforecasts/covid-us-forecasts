# Format results of weekly expert elicitation for submission/ensembling
#
# Arguments:
# for_forecast_date = character string "YYYY-MM-DD", date (Monday) of forecast submission to plot
# submission quantiles = numeric vector of quantiles needed for submission/ensembling

format_expert_elicitation = function(for_forecast_date,
                                     submission_quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)){
  

  # Cumulative data ---------------------------------------------------------
  
  # --- Get cumulative weekly data ---
  cumulative_data <- get_us_deaths(data = "cumulative")
  
  # State cumulative data
  cumulative_deaths_state <- cumulative_data %>%
    dates_to_epiweek() %>%
    dplyr::filter(epiweek_end == TRUE)
  
  # National cumulative data
  cumulative_deaths_national <- cumulative_deaths_state %>%
    dplyr::group_by(epiweek) %>%
    dplyr::summarise(deaths = sum(deaths),
                     state = "US",
                     .groups = "drop_last") %>%
    dplyr::ungroup()
  
  # Bind
  cumulative_deaths <- dplyr::bind_rows(cumulative_deaths_state, cumulative_deaths_national)
  
  # Filter to last week of data shown to model
  last_week_cumulative_deaths <- cumulative_deaths %>%
    dplyr::filter(epiweek == max(epiweek)-1) %>%
    dplyr::select(-epiweek, -date)
  
  

  # Expert forecasts --------------------------------------------------------
  
  for_forecast_date <- lubridate::floor_date(as.Date(for_forecast_date), unit = "week", week_start = 1)
  
  file_path <- here::here("expert-forecast", "raw-rds", paste0(for_forecast_date, "-agg-expert.rds"))

  raw_expert_elicitation <- readRDS(file = file_path) %>%
    dplyr::filter(forecast_date == as.Date(for_forecast_date))
  
  
  # Process expert forecasts for SHELF; save expert names
  expert_vals <- raw_expert_elicitation %>%
    dplyr::mutate(point = pmax(point, 1),
                  quantile0.05 = pmax(quantile0.05, 1),
                  quantile0.95 = pmax(quantile0.95, 1)) %>%
    dplyr::select(quantile0.05, point, quantile0.95) %>%
    t() %>%
    as.matrix()
  expert_names <- raw_expert_elicitation %>%
    select(state, target_week_end) %>%
    mutate(expert = paste0("expert.", 1:dim(expert_vals)[2]))
  
  
  # Fit distributions and retrieve submission_quantiles from the best-fit distribution
  shelf_fit <- SHELF::fitdist(vals = expert_vals, probs = c(0.05, 0.5, 0.95), lower = 0) %>%
    SHELF::feedback(quantiles = submission_quantiles) %>%
    .$expert.quantiles %>%
    set_names(paste0("expert.", 1:dim(expert_vals)[2]))
  
  
  # Bring information together
  out <- shelf_fit %>%
    mutate(quantile = as.numeric(rownames(shelf_fit))) %>%
    pivot_longer(cols = -quantile, names_to = "expert", values_to = "value") %>%
    left_join(expert_names, by = "expert") %>%
    left_join(tigris::fips_codes %>%
                dplyr::select(state_code, state_name) %>%
                unique() %>%
                rbind(c("US", "US")),
              by = c("state" = "state_name")) %>%
    left_join(last_week_cumulative_deaths %>% select(state, deaths), by = "state") %>%
    mutate(forecast_date = as.Date(for_forecast_date),
           type = "quantile",
           horizon = ceiling(as.numeric(difftime(target_week_end, forecast_date, unit = "weeks"))),
           target = paste0(horizon, " wk ahead inc death"))
  
  ## Weekly incident deaths
  inc_out <- out %>%
    dplyr::select(forecast_date, target, target_end_date = target_week_end, location = state_code, type, quantile, value)
  
  ## Weekly cumulative deaths
  cum_out <- out %>%
    dplyr::group_by(forecast_date, state_code, type, quantile) %>%
    dplyr::mutate(value = cumsum(value) + deaths,
                  target = str_replace(target, "inc", "cum")) %>%
    dplyr::select(forecast_date, target, target_end_date = target_week_end, location = state_code, type, quantile, value)
  
  ## Bind
  out <- inc_out %>%
    bind_rows(cum_out)
  
  ## Add point (median)
  out <- out %>%
    dplyr::bind_rows(out %>%
                       dplyr::filter(quantile == 0.5) %>%
                       dplyr::mutate(type = "point") %>%
                       dplyr::select(-quantile)) %>%
    dplyr::mutate(value = floor(value))
  
  
  # Save output and return
  readr::write_csv(out, path = paste0("expert-forecast/submission-files/dated/", for_forecast_date, "-expert.csv"))
  readr::write_csv(out, path = paste0("expert-forecast/submission-files/latest-expert.csv"))
  
  return(out)
    

}
