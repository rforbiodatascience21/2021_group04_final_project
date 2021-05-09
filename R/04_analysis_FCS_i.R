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
seed <- 1234
fcs_covid_data_umap <- fcs_covid_data_umap %>% 
  umap()


fcs_covid_data_umap <- fcs_covid_data_umap %>% 
  as_tibble() %>% 
  mutate(UMAP_1 = V1,
         UMAP_2 = V2, 
         .keep = "unused")

fcs_covid_data_umap <-  bind_cols(fcs_covid_data_umap, fcs_covid_data_aug) 



fcs_covid_data_umap_long <- fcs_covid_data_umap %>% 
  pivot_longer(cols = c(7:22), 
               names_to = "marker", 
               values_to = "expression")

# Visualise data ----------------------------------------------------------




# density UMAP plot

## Covid patients concatenated
fig_density_patient <- fcs_covid_data_umap %>% 
  filter(Sample != "BC10") %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_hex(bins = 70)+
  coord_fixed(ratio = 1) +
  theme_classic()+
  scale_fill_gradient2( mid = "yellow", high = "red")+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)



## HD patient
fig_density_HD <- fcs_covid_data_umap %>% 
  filter(Sample == "BC10") %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_hex(bins = 70)+
  coord_fixed(ratio = 1) +
  facet_wrap(~Sample)+
  theme_classic()+
  scale_fill_gradient2( mid = "yellow", high = "red")+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)


# Expression for every each marker

# Covid patients concatenated

fig_all_patient <- fcs_covid_data_umap_long %>% 
  filter(Sample != "BC10",
         marker %in% c("CCR7", "CD137", "CD27", "CD38", "CD39", 
                       "CD45RA", "CD57", "CD69", "Multimer-APC",
                       "HLA-DR", "PD1","CD27","Multimer-PE")) %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             color = expression))+
  geom_point(size = 0.3)+
  facet_wrap(~ marker, 
             scales = "free")+
  scale_color_viridis_c(option = "turbo")+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)
  


# HD patient

fig_all_HD <- fcs_covid_data_umap_long %>% 
  filter(Sample == "BC10",
         marker %in% c("CCR7", "CD137", "CD27", "CD38", "CD39", 
                       "CD45RA", "CD57", "CD69", "Multimer-APC",
                       "HLA-DR", "PD1","CD27","Multimer-PE")) %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             color = expression))+
  geom_point(size = 0.3)+
  facet_wrap(~ marker, 
             scales = "free")+
  scale_color_viridis_c(option = "turbo")+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)

# expression level for sigle markers

## CD45RA

fig_single_CD45RA <- fcs_covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=CD45RA)) + 
  geom_point(size = 0.3) + 
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic()+
  scale_color_viridis_c(option = "turbo") +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "CD45RA")


## CCR7

fig_single_CCR7 <- fcs_covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=CCR7)) + 
  geom_point(size = 0.3) +
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic()+
  scale_color_viridis_c(option = "turbo")+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "CCR7")


## PD1

fig_single_PD1 <- fcs_covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=PD1)) + 
  geom_point(size = 0.3) +
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic()+
  scale_color_viridis_c(option = "turbo")+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "PD1")


## CD69

fig_single_CD69 <- fcs_covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=CD69)) + 
  geom_point(size = 0.3) + 
  facet_wrap(~Sample,
             scales = "free")+
  theme_classic()+
  scale_color_viridis_c(option = "turbo")+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(colour=NA),
        aspect.ratio = 1)+
  labs(title = "CD69")



# Write data --------------------------------------------------------------

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
       height = 200, width = 200, dpi= 500)


ggsave(plot = fig_all_HD, 
       filename = "results/04_fcs_fig_all_HD.png", 
       units = "mm", 
       height = 200, width = 200, dpi= 500)

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
