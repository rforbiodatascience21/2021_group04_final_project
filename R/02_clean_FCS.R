# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)



# Load data ---------------------------------------------------------------
fcs_covid_data <- read_tsv("data/01_fcs_covid_data.tsv")



# Wrangle data ------------------------------------------------------------

fcs_covid_data_clean <- fcs_covid_data %>% 
  mutate(name = str_sub(name, start = 34)) %>% 
  separate(col = name,
           into = c("Sample", "file"),
           sep = "_") %>% 
  select(-file) %>% 
  relocate(Sample, 
           .after = Time)


# Write data --------------------------------------------------------------
write_tsv(x = fcs_covid_data_clean,
          file = "data/02_fcs_covid_data_clean.tsv")
