library(ggplot2)
library(cowplot)
library(dplyr)

plot_forecast <- function(forecasts, obs) {
  plot <- forecasts %>%
    ggplot(aes(x = date, col = model, fill = model)) +
    geom_point(data = obs, aes(y = value, fill = NULL), col = "black") +
    geom_line(data = obs, aes(y = value, fill = NULL), col = "black") +
    geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), color = NA, alpha = 0.1) +
    geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), color = NA, alpha = 0.2) +
    geom_point(aes(y = value)) +
    geom_line(aes(y = value)) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") + 
    ggplot2::labs(x = "Week ending", y = "Weekly incident deaths", 
                  col = "Model", fill = "Model") +
    theme_cowplot() +
    theme(legend.position = "bottom", text = ggplot2::element_text(family = "Sans Serif"))
}
