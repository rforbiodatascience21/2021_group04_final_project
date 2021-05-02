# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------

  
#Here you can assign your values of

covid_data_augment %>% 
  pivot_wider(names_from = SampleID,
              values_from = Fraction) %>% 
  select(-c(Patient_19:Patient_42)) 



# Model data
covid_data_augment %>% 
  filter(Last_population == c("CD38", "CD39"),
         SampleID == c("Patient_1")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = cohort_type))+
  geom_boxplot()+
  geom_point(position = position_dodge(0.75))


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)