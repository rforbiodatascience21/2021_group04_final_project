# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(uwot)

# Load data ---------------------------------------------------------------
fcs_covid_data_aug <- read_tsv("data/03_fcs_covid_data_aug.tsv")


# Wrangle data ------------------------------------------------------------
fcs_covid_data_umap <- fcs_covid_data_aug %>% 
  select(1:20) %>% 
  as.matrix()


# Model data---------------------------------------------------------------
fcs_covid_data_umap <- fcs_covid_data_umap %>% 
  umap()

fcs_covid_data_umap <- fcs_covid_data_umap %>% 
  as_tibble() %>% 
  mutate(UMAP_1 = V1,
         UMAP_2 = V2, 
         .keep = "unused")

fcs_covid_data_umap <-  bind_cols(fcs_covid_data_umap, fcs_covid_data_aug) 



# Wrangle data for ggplot -------------------------------------------------

fcs_covid_data_umap_long <- fcs_covid_data_umap %>% 
  pivot_longer(cols = c(7:22), 
               names_to = "marker", 
               values_to = "Expression")

# Visualise data ----------------------------------------------------------

point_size <- 0.3

font_size <- 14

# density UMAP plot -------------------------------------------------------

## Covid patients concatenated
fig_density_patient <- fcs_covid_data_umap %>% 
  filter(Sample != "BC10") %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_hex(bins = 70)+
  coord_fixed(ratio = 1) +
  theme_classic(base_size = font_size)+
  scale_fill_gradient2( mid = "yellow", high = "red")+
  theme_classic(base_size = font_size)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP density plot for a SARS-CoV-2 Patient")



## HD patient
fig_density_HD <- fcs_covid_data_umap %>% 
  filter(Sample == "BC10") %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_hex(bins = 70)+
  coord_fixed(ratio = 1) +
  theme_classic(base_size = font_size)+
  scale_fill_gradient2( mid = "yellow", high = "red")+
  theme_classic(base_size = font_size)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP density plot for a Healthy Donor")


# Expression for every each marker -----------------------------------

# Covid patients concatenated

fig_all_patient <- fcs_covid_data_umap_long %>% 
  filter(Sample != "BC10",
         marker %in% c("CCR7", "CD137", "CD27", "CD38", "CD39", 
                       "CD45RA", "CD57", "CD69", "CEF Multimer",
                       "HLA-DR", "PD1","CD27","SARS-CoV-2 Multimer")) %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             color = Expression))+
  geom_point(size = point_size)+
  facet_wrap(~ marker, 
             scales = "free")+
  scale_color_viridis_c(option = "turbo")+
  theme_classic(base_size = font_size)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        strip.text.x = element_text(size = 8),
        aspect.ratio = 1)+
  labs(title = "UMAP plot of SARS-CoV-2 patient",
       subtitle = "expression of biomarkers")
  


# HD patient

fig_all_HD <- fcs_covid_data_umap_long %>% 
  filter(Sample == "BC10",
         marker %in% c("CCR7", "CD137", "CD27", "CD38", "CD39", 
                       "CD45RA", "CD57", "CD69", "CEF Multimer",
                       "HLA-DR", "PD1","CD27","SARS-CoV-2 Multimer")) %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             color = Expression))+
  geom_point(size = point_size)+
  facet_wrap(~ marker, 
             scales = "free")+
  scale_color_viridis_c(option = "turbo")+
  theme_classic(base_size = font_size)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        strip.text.x = element_text(size = 8),
        aspect.ratio = 1)+
  labs(title = "UMAP plot of healthy donor",
       subtitle = "expression of biomarkers")

# expression level for sigle markers --------------------------------------



## CD45RA

fig_single_CD45RA <- fcs_covid_data_umap %>%
  mutate(Sample = recode(Sample, "AP0301" = "Patient",
                         "BC10" = "Healthy Donor")) %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2,color=CD45RA)) + 
  geom_point(size = point_size) + 
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic(base_size = font_size)+
  scale_color_viridis_c(option = "turbo") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP plot - CD45RA expression",
       color = "Expression")


## CCR7

fig_single_CCR7 <- fcs_covid_data_umap %>% 
  mutate(Sample = recode(Sample, "AP0301" = "Patient",
                         "BC10" = "Healthy Donor")) %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2,color=CCR7)) + 
  geom_point(size = point_size) +
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic(base_size = font_size)+
  scale_color_viridis_c(option = "turbo")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP plot - CCR7 expression",
       color = "Expression")


## PD1

fig_single_PD1 <- fcs_covid_data_umap %>% 
  mutate(Sample = recode(Sample, "AP0301" = "Patient",
                         "BC10" = "Healthy Donor")) %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2,color=PD1)) + 
  geom_point(size = point_size) +
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic(base_size = font_size)+
  scale_color_viridis_c(option = "turbo")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP plot - PD1 expression",
       color = "Expression")


## CD69 

fig_single_CD69 <- fcs_covid_data_umap %>% 
  mutate(Sample = recode(Sample, "AP0301" = "Patient",
                         "BC10" = "Healthy Donor")) %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=CD69)) + 
  geom_point(size = point_size) + 
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic(base_size = font_size)+
  scale_color_viridis_c(option = "turbo")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "UMAP plot - CD69 expression",
       color = "Expression")



# Write data --------------------------------------------------------------
write_tsv(x = fcs_covid_data_umap,
          file = "data/04_fcs_covid_data_umap.tsv")

ggsave(plot = fig_density_patient, 
       filename = "results/04_fcs_fig_density_patient.png", 
       units = "mm", 
       height = 200, width = 200, dpi= 500)

ggsave(plot = fig_density_HD, 
       filename = "results/04_fcs_fig_density_HD.png", 
       units = "mm", 
       height = 200, width = 200, dpi= 500)


ggsave(plot = fig_all_patient, 
       filename = "results/04_fcs_fig_all_patient.png", 
       units = "mm", 
       height = 175, width = 200, dpi= 500)


ggsave(plot = fig_all_HD, 
       filename = "results/04_fcs_fig_all_HD.png", 
       units = "mm", 
       height = 175, width = 200, dpi= 500)

ggsave(plot = fig_single_CD45RA, 
       filename = "results/04_fcs_fig_single_CD45RA.png", 
       units = "mm", 
       height = 100, width = 200, dpi= 500)

ggsave(plot = fig_single_CCR7, 
       filename = "results/04_fcs_fig_single_CCR7.png", 
       units = "mm", 
       height = 100, width = 200, dpi= 500)

ggsave(plot = fig_single_PD1, 
       filename = "results/04_fcs_fig_single_PD1.png", 
       units = "mm", 
       height = 100, width = 200, dpi= 500)

ggsave(plot = fig_single_CD69, 
       filename = "results/04_fcs_fig_single_CD69.png", 
       units = "mm", 
       height = 100, width = 200, dpi= 500)

