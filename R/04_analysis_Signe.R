# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(ggsignif)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------

#trying to take out values from patient 1-18 and HD
covid_data_augment_P18 <- covid_data_augment %>% 
  pivot_wider(names_from = SampleID,
              values_from = Fraction) %>% 
  select(-c(Patient_19:Patient_42))

covid_data_augment_P18 <- covid_data_augment_P18 %>% 
  pivot_longer(cols = c(Patient_1:`HD-2_20`),
               names_to = "SampleID",
               values_to = "Fraction")

#pivot wider for 1 line per patient

covid_data_augment_wider_CD8 <- covid_data_augment %>% 
  select(-Gating) %>% 
  filter(Parent_population == "CD8+") %>% 
  pivot_wider(names_from = Last_population,
              values_from = Fraction)

covid_data_augment_wider_multimer <- covid_data_augment %>% 
  select(-Gating) %>% 
  filter(Parent_population != "CD8+") %>% 
  pivot_wider(names_from = Last_population,
              values_from = Fraction)




# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------
#problem dont seem to show all the points?

covid_data_augment %>% 
  filter(Last_population == c("CD38", "CD39", "CD69", "HLA-DR", 
                              "PD-1", "CD27", "CD57"),
         Parent_population == "SARS_multimer+") %>% 
  mutate(Last_population = fct_relevel(Last_population,"CD38", "CD39", "CD69", 
                                       "HLA-DR", "PD-1", "CD27", "CD57")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = cohort_type))+
  geom_boxplot()+
  geom_point(position = position_dodge(0.75))

#trying to add significant stars
covid_data_augment_wider_multimer %>% 
  filter(Parent_population == "SARS_multimer+") %>% 
  ggplot(aes(x = cohort_type, 
             y = CD38))+
  geom_boxplot()+
  geom_signif(comparisons = list(c("HD-2", "Patient")), 
                              map_signif_level=TRUE)


# CEFT vs Sars_cov2 
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Last_population == c("CD38", "CD39", "CD69", "HLA-DR", 
                              "PD-1", "CD27", "CD57")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = Parent_population))+
  geom_boxplot(alpha = 0.7)+
  geom_point(position = position_dodge(0.75))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "% of multimer+ CD8+ T cells",
       fill = "")+
  scale_fill_discrete(labels = c("CEF", "SARS-CoV-2"))
  
# single plots CEF vs Sars Cov-2  
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Parent_population != "CD8+",
         Last_population == c("CD38")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = Parent_population))+
  geom_boxplot(alpha = 0.7)+
  geom_point(position = position_dodge(0.75))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "% of multimer+ CD8+ T cells",
       fill = "")+
  scale_fill_discrete(labels = c("CEF", "SARS-CoV-2"))
  

#outpaitent vs Hospitalized single marker
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Parent_population == "SARS_multimer+",
         !is.na(Hospital_status),
         Last_population == c("CD38")) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, 
                                       "Outpatient", "Hospitalized")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = Hospital_status))+
  geom_boxplot(alpha = 0.7)+
  geom_point(position = position_dodge(0.75))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "% of multimer+ CD8+ T cells",
       fill = "")

# Sars positiv out of CD8 T cells 

covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Parent_population == "CD8+",
         !is.na(Hospital_status),
         Last_population == c("SARS_multimer+")) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, 
                                       "Outpatient", "Hospitalized")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = Hospital_status))+
  geom_boxplot(alpha = 0.7)+
  geom_point(position = position_dodge(0.75))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "% of multimer+ CD8+ T cells",
       fill = "")


#facet wrap Outpaitent vs Hospitalized
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Parent_population == "SARS_multimer+",
         !is.na(Hospital_status),
         Last_population == c("CD38", "CD39", "CD69", "HLA-DR", 
                              "PD-1", "CD27", "CD57")) %>% 
  ggplot(aes(x = Hospital_status,
             y = Fraction))+
  geom_boxplot()+
  geom_point(position = position_dodge(0.75))+
  facet_wrap(~Last_population, nrow = 1)







# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)