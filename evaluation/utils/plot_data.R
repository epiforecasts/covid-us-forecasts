# Packages ----------------------------------------------------------------
library(ggplot2)
library(cowplot)
library(dplyr)

plot_data <- function(weekly, daily){
plot <- weekly %>%
    mutate(state_location = paste0(state, " (", location, ")")) %>%
    ggplot(aes(x = date, y = value, fill = NULL)) +
    geom_point(col = "black") +
    geom_line(col = "black") +
    geom_point(data = daily, col = "dark grey", size = 0.5) +
    facet_wrap(.~ state_location, scales = "free_y") +
    ggplot2::labs(x = "Week ending", y = "Weekly incident deaths", 
                  col = "Model", fill = "Model") +
    theme_cowplot() +
    theme(legend.position = "bottom", text = ggplot2::element_text(family = "Sans Serif"))
}
