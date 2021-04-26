# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_clean <- read_tsv(file = "data/02_covid_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
covid_data_augment <- covid_data_clean %>% 
  pivot_longer(cols = contains("lymphocytes"),
               names_to = "Gating",
               values_to = "Fraction")


covid_data_augment <- covid_data_augment %>% 
  separate(col = Gating,
           into = c("Lymphocytes", "Singlets", "Live_cells", "CD3",
                    "CD8", "Multimers", "Marker"),
           sep = "/",
           remove = FALSE) %>% 
  select(-c(Lymphocytes, Singlets, Live_cells, CD3, CD8))

covid_data_augment %>% 
  unite

?coalesce()


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")