## creating plots to compare 
## firstly: patient samples vs HD-1, and HD-2.
## Secondly: Hospitlized Patients vs outPatients.

## loading libraries -------------------------------
library(tidyverse)
library(dplyr)

## A plots: SARS-multimer+ populations. 
## Patient vs HD-1, vs HD-2

covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_dotplot(binaxis = "y", stackdir = "center")+
  geom_errorbar(aes(ymin = mean, color = "red"))+
  theme_classic()


covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_boxplot(outlier.shape = NA,
               color = "gray",
               fill = NA)+
  geom_jitter()+
  theme_classic()

