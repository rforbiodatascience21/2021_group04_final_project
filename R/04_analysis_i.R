# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggpubr")
library("ggsignif")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")

#creating results folder
dir.create("results")

# Visualise data ----------------------------------------------------------

text_size <- 14
dot_size <- 0.5
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


# fig A Compare HD to Patient multimer+ cells differentiation -------------

fig_A <- covid_data_augment %>% 
  filter(str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "CD45")) %>% 
  mutate(Last_population = case_when(Last_population =="CD45RA-_CCR7+" ~ "TCM",
                                     Last_population =="CD45RA+_CCR7+" ~ "Naive",
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Differentiation subsets in SARS-CoV-2 specific CD8+ T cells",
       subtitle = "-Based on expression of CD45RA and CCR7",
       y = "% of multimer+ CD8+ T cells",
       caption = "Note: P-values for one-way ANOVA (Kruskal-Wallis Test)")+
  theme(axis.title = element_text(size = 12))+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length, 
              step_increase = 0.3,
              y_position = 80)


# fig B Compare HD to Patient multimer+ cells expression of surface markers ----

fig_B <-covid_data_augment %>%
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in COVID-19 patients and HD",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length, 
              step_increase = 0.1,
              y_position = 90
              )


# fig C Compare HD to Patients co-expresion of activation markers on SARS-CoV-2 specific T cells --------------
fig_C <- covid_data_augment %>% 
  filter(str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "38\\+")) %>% 
  mutate( Last_population = recode(Last_population,
                                   "CD38+_CD39+" = "CD38+ CD39+",
                                   "CD38+_CD69+" = "CD38+ CD69+",
                                   "CD38+_HLA-DR+" = "CD38+ HLA-DR+",
                                   "CD38+_PD-1+" = "CD38+ PD-1+")) %>%
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 1)+
  labs(title = "Co-expression of CD38 and activation markers in SARS-CoV-2 specific CD8+ T cells",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              step_increase = 0.2)


#fig D line plot comparing individual marker expression between CEF and SARS multimer+ for each patient seperatly

fig_D <- covid_data_augment %>% 
  filter(
    cohort_type == "Patient",
    str_detect(Parent_population, "multimer+"),
    Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"))%>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction,
      color = Parent_population))+
  geom_point()+
  geom_line(aes(group = SampleID),
            color = "dimgray")+
  theme(legend.position = "none")+
  facet_wrap(vars(Last_population),
             scales = "free_y",
             nrow = 1, 
             strip.position = "bottom")+
  scale_y_continuous(limits=c(0,115),
                     breaks = seq(0,100, 25))+
  scale_color_manual(labels = c("CEF", "SARS-CoV-2"),
                     values = c(boxplot_color_CEF,boxplot_color_SARS_pt))+
  theme_classic(base_size = text_size)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in individual COVID-19 patients",
       y="% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("CEF_multimer+", "SARS_multimer+")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              y_position = 100)


# fig E Compare Outpatient vs Hospitalized multimer+ cells --------------

fig_E <- covid_data_augment %>% 
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 2)+
  labs(title = "SARS-CoV-2 specific CD8+ T cells",
       y = "% of multimer+ CD8+ T cells") +
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length)


# fig F Compare Outpatient vs Hospitalized multimer+ cells expression of markers--------------
fig_F <- covid_data_augment %>% 
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers COVID-19 patients",
       y="% of multimer+ CD8+ T cells")+
  theme(axis.title = element_text(size = 10))+
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              y_position = 100)


# fig G Compare Outpatient vs Hospitalized multimer+ cells co-expression of markers--------------
fig_G <- covid_data_augment %>%
  filter(cohort_type == "Patient",
         str_detect(Parent_population, "SARS"),
         str_detect(Last_population, "38\\+"),
         !is.na(Hospital_status)) %>% 
  mutate(Hospital_status = fct_relevel(Hospital_status, "Outpatient",
                                       "Hospitalized"),
         Last_population = recode(Last_population,
                                  "CD38+_CD39+" = "CD38+ CD39+",
                                  "CD38+_CD69+" = "CD38+ CD69+",
                                  "CD38+_HLA-DR+" = "CD38+ HLA-DR+",
                                  "CD38+_PD-1+" = "CD38+ PD-1+")) %>% 
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Co-expression of CD38 and activation markers in COVID-19 patients",
       y = "% of multimer+ CD8+ T cells")+
  geom_signif(comparisons = list(c("Outpatient", "Hospitalized")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              y_position = 100
              )


# fig H Compare CEF and SARS multimer+ cells expression of surface markers -----
fig_H <- covid_data_augment %>% 
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(),
        strip.background = element_rect(colour=NA),
        strip.placement = "outside",
        aspect.ratio = 2)+
  labs(title = "Expression of cell surface markers in COVID-19 patient",
       y="% of multimer+ CD8+ T cells")+
  theme(axis.title = element_text(size = 13))+
  geom_signif(comparisons = list(c("CEF_multimer+", "SARS_multimer+")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              vjust = sig_vjust,
              textsize = sig_textsize,
              color = sig_color,
              tip_length = sig_tip_length,
              y_position = 100)

# Save plots ----------------------------------------------------------

ggsave(plot = fig_A, filename = "results/04_fig_A.png", units = "mm", height = 100, width = 300, dpi= 500)
ggsave(plot = fig_B, filename = "results/04_fig_B.png", units = "mm", height = 100, width = 300, dpi= 500)
ggsave(plot = fig_C, filename = "results/04_fig_C.png", units = "mm", height = 100, width = 300, dpi= 500)
ggsave(plot = fig_D, filename = "results/04_fig_D.png", units = "mm", height = 100, width = 200, dpi= 500)
ggsave(plot = fig_E, filename = "results/04_fig_E.png", units = "mm", height = 100, width = 300, dpi= 500)
ggsave(plot = fig_F, filename = "results/04_fig_F.png", units = "mm", height = 100, width = 275, dpi= 500)
ggsave(plot = fig_G, filename = "results/04_fig_G.png", units = "mm", height = 100, width = 200, dpi= 500)
ggsave(plot = fig_H, filename = "results/04_fig_H.png", units = "mm", height = 100, width = 300, dpi= 500)
