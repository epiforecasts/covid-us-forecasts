# Packages ----------------------------------------------------------------
library(ggplot2)
library(cowplot)
library(dplyr)

plot_data <- function(data){
plot <- data %>%
    mutate(state_location = paste0(state, " (", location, ")")) %>%
    ggplot(aes(x = date, fill = NULL)) +
    geom_point(aes(y = value_weekly), col = "black") +
    geom_line(aes(y = value_weekly), col = "grey10") +
    geom_point(aes(y = value), col = "dark grey", size = 0.5) +
    ggplot2::labs(x = NULL, y = "Daily-weekly incident value", 
                  col = "Model", fill = "Model") +
    theme_cowplot() +
    theme(legend.position = "bottom", text = ggplot2::element_text(family = "Sans Serif"))
}
