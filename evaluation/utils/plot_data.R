# Packages ----------------------------------------------------------------
library(ggplot2)
library(cowplot)
library(dplyr)

plot_data <- function(data){
plot <- data %>%
    mutate(state_location = paste0(state, " (", location, ")")) %>%
    ggplot(aes(x = date, fill = NULL)) +
    geom_point(aes(y = weekly), col = "black") +
    geom_line(aes(y = weekly), col = "grey10") +
    geom_point(aes(y = daily), col = "dark grey", size = 0.5) +
    facet_wrap(~ state_location, scales = "free_y") +
    ggplot2::labs(x = "Week ending", y = "Weekly incident deaths", 
                  col = "Model", fill = "Model") +
    theme_cowplot() +
    theme(legend.position = "bottom", text = ggplot2::element_text(family = "Sans Serif"))
}
