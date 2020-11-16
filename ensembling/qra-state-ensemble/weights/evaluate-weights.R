# Evaluate weights from QRA ensembling
library(dplyr); library(ggplot2)

# get weights by week
files <- list.files(here::here("ensembling", "qra-state-ensemble", "weights"), 
                    full.names = TRUE)

weights <- grep(".rds", files, value = TRUE)
weights <- purrr::map(weights, ~ readRDS(.))

dates <- list.files(here::here("ensembling", "qra-state-ensemble", "weights"), 
                     full.names = FALSE)
dates <- grep(".rds", dates, value = TRUE)
dates <- as.Date(gsub("-qra-state-weights.rds", "", dates))

names(weights) <- dates

weights_long <- weights %>%
  purrr::keep(~ length(.x) == 3) %>%
  bind_rows(.id = "date")

weights_wide <- weights %>%
  purrr::keep(~ length(.x) > 3) %>%
  bind_rows(.id = "date") %>%
  tidyr::pivot_longer(-c(date, state), 
                      names_to = "model", 
                      values_to = "weight")

weights <- bind_rows(weights_long, weights_wide)

# Plot --------------------------------------------------------------------
# Get colours
source("utils/meta-model-list.R")
model_names <- unlist((model_list %>%
                         purrr::flatten() %>%
                         purrr::transpose())[["name"]])
weights$model <- dplyr::recode(weights$model, !!!model_names)
model_colours <- unlist((model_list %>%
                           purrr::flatten() %>%
                           purrr::transpose())[["colour"]])
names(model_colours) <- model_names

# All time weights --------------------------------------------------------

# summarise mean weight by model and date across all states
weight_by_model <- weights %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_weight = mean(weight, na.rm=T),
                   .groups = "drop")

# Plot
weight_by_model %>%
  ggplot(aes(x = model, y = mean_weight, fill = model)) +
  geom_col() +
  labs(x = NULL, y = "Mean weight in state-by-state QRA") +
  scale_fill_manual(values = model_colours) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom") 


# Facet by week -----------------------------------------------------------

# summarise mean weight by model and date across all states
weight_by_model_week <- weights %>%
  dplyr::group_by(model, date) %>%
  dplyr::summarise(mean_weight = mean(weight, na.rm=T),
                   .groups = "drop")

# Plot
weight_by_model_week %>%
  ggplot(aes(x = model, y = mean_weight, fill = model)) +
  geom_col() +
  facet_wrap(~ date) +
  labs(x = NULL, y = "Mean weight in state-by-state QRA") +
  scale_fill_manual(values = model_colours) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom") 






# All time average by state -----------------------------------------------
weight_by_state <- weights %>%
  filter(weight >= 0) %>%
  group_by(state, model) %>%
  summarise(mean_weight = mean(weight, na.rm=T),
                   .groups = "drop")
  

# Stacked bar
weight_by_state %>%
  ggplot(aes(x = state, y = mean_weight, fill = model)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = model_colours) +
  labs(x = NULL, y = "QRA state by state weight") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
        legend.position = "bottom",
        text = element_text(size = 15))




