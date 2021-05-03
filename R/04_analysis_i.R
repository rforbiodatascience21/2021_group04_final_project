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
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...

text_size <- 14
dot_size <-
dot_color <-

sig_vjust <- -0.1
sig_textsize <- 3
sig_color <- "black"
sig_tip_length <- 0


#Compare CEF and SARS multimer+ cells expression of surface markers
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "multimer"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  drop_na(Hospital_status) %>%
  ggplot(aes(x = Parent_population, y = Fraction, color = Parent_population))+
  geom_boxplot(outlier.shape = NA)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.4,
               fill = "grey",
               color = "grey")+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1)+
  scale_y_continuous(limits=c(0,105))+
  scale_color_manual(labels = c("CEF", "SARS-CoV-2"),
                     values = c("#d8b365", "#5ab4ac"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour="white"),
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("CEF_multimer+", "SARS_multimer+")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = -0.1,
              textsize = 3,
              color = "black",
              tip_length = 0)




# Copy-------------------

#Compare CEF and SARS multimer+ cells expression of surface markers
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "multimer"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  drop_na(Hospital_status) %>%
  ggplot(aes(x = Parent_population, y = Fraction, color = Parent_population))+
  geom_boxplot(outlier.shape = NA)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.4,
               fill = "grey",
               color = "grey")+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1)+
  scale_y_continuous(limits=c(0,105))+
  scale_color_manual(labels = c("CEF", "SARS-CoV-2"),
                     values = c("#d8b365", "#5ab4ac"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour="white"),
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("CEF_multimer+", "SARS_multimer+")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = -0.1,
              textsize = 3,
              color = "black",
              tip_length = 0)

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)