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

text_size <- 16
dot_size <- 0.3
dot_color <- "#696969"

boxplot_width <- 0.5

boxplot_color_CEF <- "#d8b365" 
boxplot_color_SARS_pt <- "#5ab4ac"
boxplot_color_SARS_HD1 <- "#9400D3"
boxplot_color_SARS_HD2 <- "#BF87B3"
boxplot_color_SARS_Outpt <- "blue"
boxplot_color_SARS_Hosp <- "red"

sig_vjust <- -0.1
sig_textsize <- 3
sig_color <- "black"
sig_tip_length <- 0




# fig A Compare HD to Patient multimer+ cells expression of surface markers ----

covid_data_augment %>% 
  filter(str_detect(Parent_population, "SARS"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  ggplot(aes(x = cohort_type, 
             y = Fraction, 
             color = cohort_type))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limit = c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(labels = c("HD1", "HD2", "Patient"),
                     values = c(boxplot_color_SARS_HD1,
                                boxplot_color_SARS_HD2,
                                boxplot_color_SARS_pt))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length, 
              step_increase = 0.1
              )



# fig B Compare CEF and SARS multimer+ cells expression of surface markers -----
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "multimer"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  drop_na(Hospital_status) %>%
  ggplot(aes(x = Parent_population, 
             y = Fraction, 
             color = Parent_population))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limits=c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(labels = c("CEF", "SARS-CoV-2"),
                     values = c(boxplot_color_CEF,boxplot_color_SARS_pt))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("CEF_multimer+", "SARS_multimer+")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length)

# fig D Compare Outpatient vs Hospitalized multimer+ cells --------------

covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Last_population, "SARS"),
         !is.na(Hospital_status)) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, "Outpatient",
                                       "Hospitalized")) %>% 
  ggplot(aes(x = Hospital_status, 
             y = Fraction, 
             color = Hospital_status))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  scale_color_manual(values = c(boxplot_color_SARS_Outpt,
                                boxplot_color_SARS_Hosp))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 2)+
  labs(title = "SARS-CoV-2 Multimer+ CD8 T cells",
       y = "% of CD8+ T cells")+
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length)




# fig E Compare Outpatient vs Hospitalized multimer+ cells expression of markers--------------
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "SARS"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", 
                                "CD69", "HLA-DR", "PD-1"),
         !is.na(Hospital_status)) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, "Outpatient",
                                       "Hospitalized")) %>% 
  ggplot(aes(x = Hospital_status, 
             y = Fraction, 
             color = Hospital_status))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limits=c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(values = c(boxplot_color_SARS_Outpt,
                                boxplot_color_SARS_Hosp))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length)

# fig F Compare Outpatient vs Hospitalized multimer+ cells co-expression of markers--------------
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "38\\+"),
         !is.na(Hospital_status)) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, "Outpatient",
                                       "Hospitalized")) %>% 
  ggplot(aes(x = Hospital_status, 
             y = Fraction, 
             color = Hospital_status))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limits=c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(values = c(boxplot_color_SARS_Outpt,
                                boxplot_color_SARS_Hosp))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length)


# fig S7B Compare HD to Patient multimer+ cells differentiation----

covid_data_augment %>% 
  filter(str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "CD45")) %>% 
  mutate(Last_population = case_when(Last_population =="CD45RA-_CCR7+" ~ "TCM",
                                     Last_population =="CD45RA+_CCR7+" ~ "Naïve",
                                     Last_population =="CD45RA+_CCR7-" ~ "TEMRA",
                                     Last_population =="CD45RA-_CCR7-" ~ "TEM")) %>% 
  ggplot(aes(x = cohort_type, 
             y = Fraction, 
             color = cohort_type))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limit = c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(labels = c("HD1", "HD2", "Patient"),
                     values = c(boxplot_color_SARS_HD1,
                                boxplot_color_SARS_HD2,
                                boxplot_color_SARS_pt))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Differentiation subsets in multimer+ CD8 T cells",
       subtitle = "based on expression of CD45RA and CCR7",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length, 
              step_increase = 0.1)

# fig S8 C Compare Outpatient vs Hospitalized multimer+ cells co-expression of markers--------------
covid_data_augment %>% 
  filter(str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "38\\+"))%>%  
  ggplot(aes(x = cohort_type, 
             y = Fraction, 
             color = cohort_type))+
  geom_boxplot(outlier.shape = NA,
               width = boxplot_width)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = dot_size,
               fill = NA,
               color = dot_color)+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limits=c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(labels = c("HD1", "HD2", "Patient"),
                     values = c(boxplot_color_SARS_HD1,
                                boxplot_color_SARS_HD2,
                                boxplot_color_SARS_pt))+
  theme_classic(base_size = text_size)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 1)+
  labs(title = "Expression of cell surface markers in multimer+ CD8 T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              step_increase = 0.1)


# Copy-------------------
covid_data_augment %>% 
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "multimer"),
         Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1")) %>% 
  drop_na(Hospital_status) %>%
  ggplot(aes(x = Parent_population, y = Fraction, color = Parent_population))+
  geom_boxplot(outlier.shape = NA,
               width = 0.5)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.3,
               fill = NA,
               color = "#696969")+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1)+
  scale_y_continuous(limits=c(0,105))+
  scale_color_manual(labels = c("CEF", "SARS-CoV-2"),
                     values = c("#d8b365", "#5ab4ac"))+
  theme_classic(base_size = 16)+
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





#O:\Public\T-cells-and-cancer\SRH group\Group members\COVID team\Susana_FCS files


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)