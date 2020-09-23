# Plot results of weekly expert elicitation
#
# Arguments:
# for_forecast_date = character string "YYYY-MM-DD", date (Monday) of forecast submission to plot
# individual = TRUE/FALSE, whether to include individual experts' point estimates on plot

plot_expert = function(for_forecast_date, individual = FALSE){
  

  # Observed data -----------------------------------------------------------
  
  ## Load daily data
  deaths_data <- get_us_deaths(data = "daily")
  
  state_weekly <- deaths_data %>%
    dates_to_epiweek() %>%
    dplyr::filter(epiweek_full == TRUE) %>%
    dplyr::mutate(target_week_end = lubridate::floor_date(date, unit = "week", week_start = 1) + 6) %>%
    dplyr::group_by(state, target_week_end) %>%
    dplyr::summarise(point = sum(deaths, na.rm = TRUE)) %>%
    dplyr::mutate(model = "Observed data")
  
  national_weekly <- state_weekly %>%
    dplyr::group_by(model, target_week_end) %>%
    dplyr::summarise(point = sum(point, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(state = "US")
  
  weekly_data <- state_weekly %>%
    dplyr::bind_rows(national_weekly)
  
  
  # Rt forecasts ------------------------------------------------------------

  load_addr <- "https://raw.githubusercontent.com/epiforecasts/covid-us-forecasts/master/rt-forecast-2/output/fixed_rt/submission-files/dated/"
  raw_data <- readr::read_csv(file = paste0(load_addr, for_forecast_date, "-rt-2-forecast.csv"))
  
  forecast_data <- raw_data %>%
    tibble() %>%
    filter(quantile %in% c(0.05, 0.5, 0.95),
           grepl("inc", target)) %>%
    mutate(horizon = as.numeric(substr(target, 1, 2)),
           model = "Rt forecast",
           q_type = paste0(type, quantile),
           forecast_date = as.Date(forecast_date),
           target_end_date = as.Date(target_end_date)) %>%
    filter(horizon <= 4) %>%
    left_join(tigris::fips_codes %>%
                dplyr::select(state_code, state_name) %>%
                unique() %>%
                rbind(c("US", "US")),
              by = c("location" = "state_code")) %>%
    select(model, forecast_date, target_week_end = target_end_date, state = state_name, q_type, value) %>%
    pivot_wider(id_cols = c(model, forecast_date, target_week_end, state), names_from = q_type, values_from = value) %>%
    mutate(point = quantile0.5) %>%
    select(-quantile0.5)
  

  # Expert forecasts --------------------------------------------------------

  agg_expert <- readRDS(file = here::here("expert-forecast", "raw-rds", paste0(for_forecast_date, "-agg-expert.rds"))) %>%
    dplyr::filter(forecast_date == as.Date(for_forecast_date))
  ind_expert <- readRDS(file = here::here("expert-forecast", "raw-rds", paste0(for_forecast_date, "-ind-expert.rds"))) %>%
    dplyr::filter(forecast_date == as.Date(for_forecast_date))
  
  expert_states <- agg_expert %>%
    .$state %>%
    unique()
  

  # Combine and visualise ---------------------------------------------------

  combined_data <- forecast_data %>%
    dplyr::bind_rows(agg_expert) %>%
    dplyr::bind_rows(ind_expert) %>%
    dplyr::bind_rows(weekly_data) %>%
    dplyr::filter(state %in% expert_states) %>%
    dplyr::filter(target_week_end >= as.Date("2020-06-01"))
    
  
  ## Plotting
  bpal <- brewer.pal(3, name = "Set2")
  cols <- c("Individual experts" = bpal[1], "Expert consensus" = bpal[2], "Rt forecast" = "grey60", "Observed data" = "black")
  plot_dates <- seq.Date(from = min(combined_data$target_week_end),
                         to = max(combined_data$target_week_end),
                         by = "2 weeks")

  g <- combined_data %>%
    filter(model != "Individual experts") %>%
    ggplot(aes(x = target_week_end, y = point, col = model, fill = model)) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = quantile0.05, ymax = quantile0.95), col = NA, alpha = 0.2) +
    geom_line(lwd = 0.6) +
    facet_wrap(.~ state, scales = "free_y") +
    #
    scale_x_date(breaks = plot_dates, date_labels = "%d %b") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    labs(x = "Week", y = "Weekly incident deaths",
         title = paste0("Expert forecasts made in week beginning ", lubridate::floor_date(as.Date(for_forecast_date), unit = "week", week_start = 7)),
         col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    theme(legend.position = "top") 
  
  if(individual){
    
   g <- g + 
      geom_point(data = combined_data %>% filter(model %in% c("Individual experts")), size = 2, alpha = 0.5)
    
  }
  
  return(g)
  
}
