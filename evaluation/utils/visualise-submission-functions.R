plot_forecasts = function(national = TRUE,
                          state_min_cutoff = 5,
                          obs_weeks = 5,
                          exclude_new_epiweek = FALSE,
                          models = "all"){
  
  
  # Get observed data ------------------------------------------------------------------
  source(here::here("utils", "get-us-data.R"))
  
  daily_deaths_state <- get_us_deaths(data = "daily") %>%
    dplyr::mutate(day = ordered(weekdays(as.Date(date)), 
                                levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                  epiweek_day = as.numeric(paste0(epiweek, ".", as.numeric(day))))
  
  
  # Optional: filter out data from the last incomplete week
  if(exclude_new_epiweek){
    daily_deaths_state <- daily_deaths_state %>%
      dplyr::filter(epiweek_day < max(epiweek))
  }
  
  weekly_deaths_state <- daily_deaths_state %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
    dplyr::group_by(state, epiweek) %>%
    dplyr::summarise(deaths = sum(deaths),
                     target_end_date = max(date), .groups = "drop_last") %>%
    dplyr::ungroup()
  
  weekly_deaths_national <- weekly_deaths_state %>%
    dplyr::group_by(epiweek, target_end_date) %>%
    dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
    dplyr::ungroup()
  
  
  # Get forecasts -----------------------------------------------------------
  
  # source function
  source(here::here("utils", "load-submissions-function.R"))
  model_list <- readRDS(here::here("utils", "model_list.rds"))
  
  ## Get most recent Rt forecast 
  forecasts <- load_submission_files(dates = "latest",
                                     models = models)
  
  # Rename models for nicer plots
  model_names <- unlist((model_list %>%
                           purrr::flatten() %>%
                           purrr::transpose())[["name"]])
  forecasts$model <- dplyr::recode(forecasts$model, !!!model_names)
  
  # and get model colours
  model_colours <- unlist((model_list %>%
                           purrr::flatten() %>%
                           purrr::transpose())[["colour"]])
  names(model_colours) <- model_names

  
  # Reshape forecasts and add observed data --------------------------------------------------------------------
  # Filter to incidence forecasts and pivot forecasts for plotting
  forecasts_state <- forecasts %>%
    dplyr::filter(grepl("inc", target)) %>%
    dplyr::group_by(state, target_end_date, model) %>%
    dplyr::mutate(quantile = stringr::str_c("c", quantile)) %>%
    dplyr::filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
    tidyr::pivot_wider(id_cols = c(state, target_end_date, model), 
                       names_from = quantile, values_from = value) %>%
    dplyr::ungroup() 
  
  forecasts_national <- forecasts %>%
    dplyr::filter(state %in% "US",
                  grepl("inc", target)) %>%
    dplyr::group_by(state, target_end_date, model) %>%
    dplyr::mutate(quantile = stringr::str_c("c", quantile)) %>%
    dplyr::filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
    tidyr::pivot_wider(id_cols = c(state, target_end_date, model), names_from = quantile, values_from = value) %>%
    dplyr::ungroup()
  
  # Set observed data to match format
  observed_deaths_state <- dplyr::filter(weekly_deaths_state) %>%
    dplyr::mutate(model = "Observed") %>%
    dplyr::filter(epiweek >= (max(epiweek) - obs_weeks)) %>% 
    dplyr::select(-epiweek, c0.5 = deaths)
  
  observed_deaths_national <- weekly_deaths_national %>%
    dplyr::mutate(model = "Observed") %>%
    dplyr::filter(epiweek >= (max(epiweek) - obs_weeks)) %>% 
    dplyr::select(-epiweek, c0.5 = deaths)
  
  # Identify and filter which states to keep -------------------------------------------
  if (national) {
    plot_national <- bind_rows(forecasts_national, observed_deaths_national) %>%
      dplyr::mutate(state = "US")
    
    plot_df <- plot_national
    
  } else { 
    
    # Identify over 5 cases in the last week
    source(here::here("utils", "states-min-last-week.R"))
    keep_states <- states_min_last_week(min_last_week = state_min_cutoff, last_week = 1)
    
    plot_state <- dplyr::bind_rows(forecasts_state, observed_deaths_state) %>%
      dplyr::filter(state %in% keep_states$state)
    
    plot_df <- plot_state
    
  }
  
  # Order models as factor
  plot_df$model <- factor(plot_df$model, levels = c("Observed", model_names))
  
  # Ensure models and their plotting colours match up
  model_colours <- model_colours[names(model_colours) %in% unique(plot_df$model)]
  
  # Set plot
  plot <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = target_end_date, col = model, fill = model)) +
    ggplot2::geom_point(ggplot2::aes(y = c0.5), size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = c0.5), lwd = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = c0.25, ymax = c0.75), color = NA, alpha = 0.1) +
    ##
    ggplot2::scale_fill_manual(values = c("grey", model_colours)) +
    ggplot2::scale_color_manual(values = c("dark grey", model_colours)) +
    ggplot2::facet_wrap(.~ state, scales = "free_y") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "Week ending", y = "Weekly incident deaths", 
                  caption = paste0("States with deaths last week >", state_min_cutoff),
                  col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom", 
                   text = ggplot2::element_text(family = "Sans Serif"))
  
  return(plot)
}
