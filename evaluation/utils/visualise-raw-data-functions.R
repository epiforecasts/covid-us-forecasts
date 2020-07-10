plot_raw_data = function(national = TRUE,
                         cutoff = 25){
  
  # Get observed data ------------------------------------------------------------------
  daily_deaths_state <- get_us_deaths(data = "daily") %>%
    dplyr::mutate(day = ordered(weekdays(as.Date(date)), 
                                levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                  epiweek_day = as.numeric(paste0(epiweek, ".", as.numeric(day)))) %>%
    # filter out data from the last incomplete week
    dplyr::filter(epiweek_day < max(epiweek))
  
  
  weekly_deaths_state <- daily_deaths_state %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
    dplyr::group_by(state, epiweek) %>%
    dplyr::summarise(deaths = sum(deaths),
                     target_end_date = max(date)) %>%
    dplyr::ungroup()
  
  weekly_deaths_national <- weekly_deaths_state %>%
    dplyr::group_by(epiweek, target_end_date) %>%
    dplyr::summarise(deaths = sum(deaths)) %>%
    dplyr::ungroup()
  
  
  
  # Set observed data to match format
  observed_deaths_state <- dplyr::filter(weekly_deaths_state) %>%
    dplyr::mutate(model = "Observed") %>%
    dplyr::select(-epiweek, c0.5 = deaths)
  
  observed_deaths_national <- weekly_deaths_national %>%
    dplyr::mutate(model = "Observed",
                  state = "US") %>%
    dplyr::select(-epiweek, c0.5 = deaths)
  
  # Identify and filter which states to keep -------------------------------------------
  
  # Identify over 100 cases in the last week
  keep_states <- states_min_last_week(min_last_week = cutoff, last_week = 1)
  
  plot_state <- observed_deaths_state %>%
    dplyr::filter(state %in% keep_states$state) 
  
  plot_national <- observed_deaths_national %>%
    dplyr::mutate(state = "US")
  
  if (national) {
    plot_df <- plot_national
  } else {
    plot_df <- plot_state
  }
  
  plot <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = target_end_date, col = model, fill = model)) +
    ggplot2::geom_point(ggplot2::aes(y = c0.5), size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = c0.5), lwd = 1) +
    ggplot2::scale_fill_manual(values = c("grey", RColorBrewer::brewer.pal(4, name = "Set2"))) +
    ggplot2::scale_color_manual(values = c("dark grey", RColorBrewer::brewer.pal(4, name = "Set2"))) +
    ggplot2::facet_wrap(.~ state, scales = "free_y") +
    ggplot2::labs(x = "Week ending", y = "Weekly incident deaths",
         col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom", 
                   text = ggplot2::element_text(family = "Sans Serif"))
  
  return(plot)
}

