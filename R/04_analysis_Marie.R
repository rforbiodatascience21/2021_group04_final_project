# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------
#my_data_clean_aug %>% ...


# Model data
#my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

# Compare expresion of surface markers in SARS-CoV-2 mult+ CD8+ T cells in all cohorts
covid_data_augment %>% 
  filter(Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
         str_detect(Parent_population, "SARS")) %>% 
  ggplot(aes(x = cohort_type, y = Fraction, color = cohort_type))+
  geom_boxplot()+
  geom_jitter(alpha = 0.8)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = "Expression of cell surface markers",
       y = "% of SARS-CoV-2 mult+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              margin_top = 0.005,
              step_increase = 0.1,
              vjust = 0.7,
              textsize = 3)


# Compare co-expression with CD38 in all cohorts
covid_data_augment %>% 
  filter(str_detect(Last_population, "CD38\\+"),
         str_detect(Parent_population, "SARS")) %>% 
  ggplot(aes(x = cohort_type, y = Fraction, color = cohort_type))+
  geom_boxplot()+
  geom_jitter(alpha = 0.8)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             ncol = 4)+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = "Co-expression of cell surface markers",
       y = "% of SARS-mult+ CD8+ T cells")

# Compare CD8 T cell subsets in corhorts
covid_data_augment %>% 
  filter(str_detect(Last_population, "CD45RA")) %>% 
  ggplot(aes(x = cohort_type, y = Fraction, color = cohort_type))+
  geom_boxplot()+
  geom_jitter(alpha = 0.8)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             ncol = 4)+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = "Co-expression of cell surface markers",
       y = "% of SARS-CoV-2 mult+ CD8+ T cells")

#Compare Hospital status in patients by expression of surface markers
covid_data_augment %>% 
filter(cohort_type == "Patient",
       str_detect(Parent_population, "SARS")) %>% 
  drop_na(Hospital_status) %>% 
  ggplot(aes(x = Hospital_status, y = Fraction, color = Hospital_status))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(vars(Last_population),
             scales = "free_y")+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = "Expression of cell surface markers in patients",
       y = "% of SARS-CoV-2 mult+ CD8+ T cells")

#Compare CEF and SARS multimer+ cells expression of surface markers
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "multimer"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  drop_na(Hospital_status) %>%
  ggplot(aes(x = Parent_population, y = Fraction, color = Parent_population))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             ncol = 4)+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")



# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
