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

covid_data_augment %>% 
  filter(Last_population =="SARS_multimer+") %>% filter(!is.na (Fraction)) %>%
  ggplot(mapping = aes(x=Hospital_status, y = Fraction, color = Hospital_status)
  )+
  geom_boxplot(outlier.shape=1, outlier.size=2)+
  stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
  stat_summary(fun = mean, geom="point", size=3)+
  geom_jitter(size=2, shape=1, position=position_jitter(0.0))+
  labs(title="Frequencies of SARS-CoV-2 multimer+ CD8+ T cells in outpatient and hospitalized patients", 
       y = ("SARS-CoV-2 specific T cells \n (% of multimer+ CD8+ Tcells)"))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank()
  )


covid_data_augment %>% 
  filter(cohort_type =="Patient",Parent_population =="SARS_multimer+", Last_population %in% c("CD38", "CD39", "HLA-DR", "PD-1")) %>%
  ggplot(mapping = aes(x=Hospital_status, y = Fraction, color = Hospital_status)
  )+
  geom_boxplot(outlier.shape=1, outlier.size=2)+
  stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
  stat_summary(fun = mean, geom="point", size=3)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 2)+
  geom_jitter(size=2, shape=1, position=position_jitter(0.0))+
  labs(title="Percentage of SARS-CoV-2 pMHC multimer positive CD8+ T cells expressing the indicated surface markers in outpatients
and hospitalized patients", y = "% of multimer+ CD8+ Tcells")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank()
  )

covid_data_augment %>% 
  filter(Parent_population =="SARS_multimer+", Last_population %in% c("CD38", "CD39", "HLA-DR", "PD-1")) %>% filter(!is.na (Hospital_status)) %>%
  ggplot(mapping = aes(x=Hospital_status, y = Fraction, color = Hospital_status)
  )+
  geom_boxplot(outlier.shape=1, outlier.size=2)+
  stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
  stat_summary(fun = mean, geom="point", size=3)+
  geom_jitter(size=2, shape=1, position=position_jitter(0.0))+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 2)+
  labs(title="SARS-CoV-2 pMHC multimer positive CD8+ T cells \n expressing the indicated surface markers in outpatients and hospitalized patients", 
       y = "% of multimer+ CD8+ Tcells")+
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