# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data <- read_tsv(file = "data/01_covid_data.tsv")


# Wrangle data ------------------------------------------------------------
covid_data_clean <- covid_data %>% 
  na_if("NA") %>% 
  mutate_if(is.numeric, round, digits=1)

covid_data_clean <- covid_data %>% 
  mutate(cohort_type = case_when( str_detect(SampleID, "Patient") ~ "Patient",
                                  str_detect(SampleID, "HD-1") ~ "HD-1",
                                  str_detect(SampleID, "HD-2") ~"HD-2"),
         .after = SampleID,
         cohort_type = factor(cohort_type, 
                              levels = c("Patient", "HD-1", "HD-2"))
         )


# Write data --------------------------------------------------------------
write_tsv(x = covid_data_clean,
          file = "data/02_covid_data_clean.tsv")
