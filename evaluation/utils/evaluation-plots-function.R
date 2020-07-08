# Plot Scores
# Takes in summarised scores from eval_forecasts
# what is currently still a bit tricky is the double use of by, 
# once in scoringutils::eval_forecasts and once in plot_scores. 
# The reason for that is that eval_forecasts definitely needs the by argument 
# to compute bias and calibration and sharpness, but it can't compute quantiles
# yet so we need another by to define the groups for which quantiles 
# should be computed. Want to eventualy also move the quantiles
# as an option for the package, but suggest to use it as is for now. 

# facet_formula, y, and colour get passed down to the interval plot and 
# the calibration plot, dodge_wdith gets passed down to all plots

plot_scores <- function(scores, 
                        y = "horizon", 
                        colour = "model", 
                        by = c("model", "range", "horizon"), 
                        facet_formula = ~ range, 
                        dodge_width = 0.4) {
  
  data.table::setDT(scores)
  
  ## average / take quantiles over states
  plot_df <- scores[, .(lower25_score = quantile(Interval_Score, 0.25, na.rm = TRUE),
                        upper75_score = quantile(Interval_Score, 0.75, na.rm = TRUE),
                        lower05_score = quantile(Interval_Score, 0.05, na.rm = TRUE),
                        upper95_score = quantile(Interval_Score, 0.95, na.rm = TRUE),
                        Interval_Score = mean(Interval_Score),
                        lower25_calibration = quantile(calibration, 0.25, na.rm = TRUE),
                        upper75_calibration = quantile(calibration, 0.75, na.rm = TRUE),
                        lower05_calibration = quantile(calibration, 0.05, na.rm = TRUE),
                        upper95_calibration = quantile(calibration, 0.95, na.rm = TRUE),
                        calibration = mean(calibration),
                        lower25_bias = quantile(bias, 0.25, na.rm = TRUE),
                        upper75_bias = quantile(bias, 0.75, na.rm = TRUE),
                        lower05_bias = quantile(bias, 0.05, na.rm = TRUE),
                        upper95_bias = quantile(bias, 0.95, na.rm = TRUE),
                        bias = mean(bias),
                        lower25_sharpness = quantile(sharpness, 0.25, na.rm = TRUE),
                        upper75_sharpness = quantile(sharpness, 0.75, na.rm = TRUE),
                        lower05_sharpness = quantile(sharpness, 0.05, na.rm = TRUE),
                        upper95_sharpness = quantile(sharpness, 0.95, na.rm = TRUE), 
                        sharpness = mean(sharpness)), by = by]
  
  # add grouping variables
  plot_df[, `:=` (y = forcats::fct_rev(as.factor(get(y))), 
                  colour = get(colour))]
  
  plots <- list()
  
  # interval plot
  plots[["interval_score_plot"]] <- 
  ggplot2::ggplot(plot_df, ggplot2::aes(y = y, colour =  colour)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = log(lower25_score), xmax = log(upper75_score)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = log(lower05_score), xmax = log(upper95_score)),
                            size = 2,
                            alpha = 0.4, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_point(ggplot2::aes(x = log(Interval_Score)),
                        colour = "black",
                        shape = 3,
                        position = ggplot2::position_dodge2(width = dodge_width, 
                                                            padding = 0)) + 
    ggplot2::facet_grid(facet_formula, scales = "free",
                        labeller = "label_both") +
    cowplot::theme_cowplot() +
    ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                   legend.position = "bottom") + 
    ggplot2::labs(colour = colour) + 
    ggplot2::ylab(y) + 
    ggplot2::xlab("Log Interval Score")
  
  
  # calibration plot
  plots[["calibration_plot"]] <-  
  ggplot2::ggplot(plot_df[range != 0, ], 
                  ggplot2::aes(y = y, colour = colour)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower25_calibration), xmax = (upper75_calibration)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower05_calibration), xmax = (upper95_calibration)),
                            size = 2,
                            alpha = 0.4, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_point(ggplot2::aes(x = calibration),
                        colour = "black",
                        shape = 3,
                        position = ggplot2::position_dodge2(width = dodge_width, 
                                                            padding = 0)) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = range/100), colour = "gray") +
    ggplot2::facet_grid(facet_formula, scales = "free",
                        labeller = "label_both") +
    cowplot::theme_cowplot() +
    ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                   legend.position = "bottom") + 
    ggplot2::labs(colour = colour) + 
    ggplot2::ylab(y) + 
    ggplot2::xlab("Calibration")
  

  # bias plot
  plots[["bias_plot"]] <- 
  ggplot2::ggplot(plot_df[range == 0, ], 
                  ggplot2::aes(y = y, colour = colour)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower25_bias), xmax = (upper75_bias)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower05_bias), xmax = (upper95_bias)),
                            size = 2,
                            alpha = 0.4, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_point(ggplot2::aes(x = bias),
                        colour = "black",
                        shape = 3,
                        position = ggplot2::position_dodge2(width = dodge_width, 
                                                            padding = 0)) + 
    ggplot2::facet_grid(scales = "free",
                        labeller = "label_both") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), colour = "grey") + 
    cowplot::theme_cowplot() +
    ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                   legend.position = "bottom") + 
    ggplot2::labs(colour = colour) + 
    ggplot2::ylab(y) + 
    ggplot2::xlab("Bias")
  
  
  # sharpness plot
  plots[["sharpness_plot"]] <- 
    ggplot2::ggplot(plot_df[range == 0, ], 
                    ggplot2::aes(y = y, colour = colour)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower25_sharpness), xmax = (upper75_sharpness)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (lower05_sharpness), xmax = (upper95_sharpness)),
                            size = 2,
                            alpha = 0.4, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_point(ggplot2::aes(x = sharpness),
                        colour = "black",
                        shape = 3,
                        position = ggplot2::position_dodge2(width = dodge_width, 
                                                            padding = 0)) + 
    cowplot::theme_cowplot() +
    ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                   legend.position = "bottom") + 
      ggplot2::labs(colour = colour) + 
      ggplot2::ylab(y) + 
      ggplot2::xlab("Sharpness")
  
  return(plots)
}




