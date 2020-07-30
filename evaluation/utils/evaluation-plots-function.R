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
                        facet_formula = ~ range, 
                        dodge_width = 0.4) {
  
  # add grouping variables
  plot_df <- data.table::copy(scores)
  
  plot_df[, `:=` (y = forcats::fct_rev(as.factor(get(y))), 
                  colour = get(colour))]
  
  plots <- list()
  
  # interval plot
  plots[["interval_score_plot"]] <- 
  ggplot2::ggplot(plot_df, ggplot2::aes(y = y, colour =  colour)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = log(interval_score_0.25), 
                                         xmax = log(interval_score_0.75)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = log(interval_score_0.05), xmax = log(interval_score_0.95)),
                            size = 2,
                            alpha = 0.4, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_point(ggplot2::aes(x = log(interval_score)),
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
    ggplot2::geom_linerange(ggplot2::aes(xmin = (calibration_0.25), xmax = (calibration_0.75)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (calibration_0.05), xmax = (calibration_0.95)),
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
    ggplot2::geom_linerange(ggplot2::aes(xmin = (bias_0.25), xmax = (bias_0.95)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (bias_0.05), xmax = (bias_0.95)),
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
    ggplot2::geom_linerange(ggplot2::aes(xmin = (sharpness_0.25), xmax = (sharpness_0.75)), 
                            size = 2, 
                            position = ggplot2::position_dodge2(width = dodge_width, 
                                                                padding = 0)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = (sharpness_0.05), xmax = (sharpness_0.95)),
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




