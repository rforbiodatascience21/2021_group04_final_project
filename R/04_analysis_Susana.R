# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

ggplot(data = filter(covid_data_augment,Parent_population =="SARS_multimer+",Last_population == "CD38"),
  mapping = aes(x=cohort_type, y = Fraction, color = cohort_type)
  )+
  geom_boxplot(outlier.shape=1, outlier.size=2)+
  stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
  stat_summary(fun = mean, geom="point", size=3)+
  geom_jitter(size=2, shape=1, position=position_jitter(0.0))+
  labs(title="Phenotype profile of SARS-CoV-2 specific T cells",x="CD38", y = "% of multimer+ CD8+ Tcells")+
  theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title = element_blank())

covid_data_augment %>% 
  filter(Parent_population =="SARS_multimer+", Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>%
           ggplot(mapping = aes(x=cohort_type, y = Fraction, color = cohort_type)
           )+
           geom_boxplot(outlier.shape=1, outlier.size=2)+
           stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
           stat_summary(fun = mean, geom="point", size=3)+
           geom_jitter(size=2, shape=1, position=position_jitter(0.0))+
           facet_wrap(vars(Last_population),
                      scales = "free_y",
                      nrow = 2)+
           labs(title="Phenotype profile of SARS-CoV-2 specific T cells", y = "% of multimer+ CD8+ Tcells")+
           theme_bw()+
           theme(axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 legend.title = element_blank(),
                 axis.title.x = element_blank(),
                 strip.background = element_rect(colour="white")
           )


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)