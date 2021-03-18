# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("purrr")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
gravier_clean_aug <- read_tsv(file = "data/03_gravier_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
gravier_clean_aug_long <- gravier_clean_aug %>%
  select(-value) %>%
  pivot_longer(cols = contains("g"),
               names_to = "gene",
               values_to = "log2_expr_level")


set.seed(7)
gravier_clean_aug_long <- gravier_clean_aug_long %>%
  sample_n(size = 100)


gravier_data_wide = gravier_clean_aug %>%
  select(response, pull(gravier_clean_aug_long, gene))


pca_fit <- gravier_data_wide %>% 
  select(-response) %>% 
  prcomp(scale = TRUE)



# Visualise data ----------------------------------------------------------

PCA_plot <- pca_fit %>%
  augment(gravier_data_wide) %>% 
  mutate(response = factor(response)) %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = response)) + 
  geom_point(size = 1.5) +
  theme_classic(base_family = "Avenir",
                base_size = 10)+
  theme(legend.position = "bottom")+
  labs(title = "PCA plot")

# Write data --------------------------------------------------------------
write_tsv(x = gravier_data_wide,
          path = "data/04_gravier_data_wide.tsv.gz")
ggsave(filename = "results/04_PCA_plot.png", 
       width = 16, 
       height = 9, 
       dpi = 72)
