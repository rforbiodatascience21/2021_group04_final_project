# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggpubr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------
#my_data_clean_aug %>% ...


# Model data
#my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

#Expression of cell surface markers in patient - Violin plot along with box plot
#Most of the codes are a copy of Marie's code for boxplot generation

covid_data_augment %>% 
  filter(Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
         str_detect(Parent_population, "SARS")) %>%
  ggplot(aes(x = cohort_type,
             y = Fraction,
             color = cohort_type))+
  geom_violin(position = "dodge")+
  geom_boxplot(width=.1, outlier.colour=NA, position = "dodge")+
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


#Plotting a correlation plot for the cell surface markers detected in 
#hospitalized vs outpatients

covid_data_augment %>% 
  filter(Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
         str_detect(Parent_population, "SARS")) %>% 
ggscatter(aes(x = "Hospitalized", 
              y = "Outpatient",
            add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson"))+
facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1)

# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)