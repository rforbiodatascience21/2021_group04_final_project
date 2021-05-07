# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
covid_data_clean <- read_tsv(file = "data/02_covid_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
covid_data_augment <- covid_data_clean %>% 
  pivot_longer(cols = contains("lymphocytes"),
               names_to = "Gating",
               values_to = "Fraction") %>% 
  mutate(T_cell = case_when(str_detect(Gating, "CD8") ~ "CD8",
                            str_detect(Gating, "CD4") ~ "CD4"),
         Parent_population = str_match(Gating, "\\S+\\/(\\S+)\\/\\S+$")[,2],
         Last_population = str_match(Gating, "\\S+\\/(\\S+)$")[,2],
         .before = Fraction)




# Write data --------------------------------------------------------------
write_tsv(x = covid_data_augment,
          file = "data/03_covid_data_augment.tsv")
