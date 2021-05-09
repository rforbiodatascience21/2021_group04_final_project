if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")


BiocManager::install("flowCore")

install.packages("tidyverse")
install.packages("uwot")


library(flowCore)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(uwot)



# reading file by file-----------------------------------------------------------------

covid_files <- list.files(path = "data/_raw/CD8_gated_files/", 
                    pattern = "\\.fcs$",
                    full.names = TRUE)


covid_data <- covid_files %>% 
  set_names() %>% 
  map_dfr(.f = ~read.FCS(filename = .x,
                         transformation = FALSE,
                         truncate_max_range = FALSE) %>% 
            exprs() %>% 
            as_tibble(), 
          .id = "name"
  )


covid_data <- covid_data %>% 
  group_by(name) %>% 
  sample_n(4000)
  
  
covid_data <- covid_data %>% 
  mutate(name = str_sub(name, start = 34)) %>% 
  separate(col = name,
           into = c("Sample", "file"),
           sep = "_") %>% 
  select(-file) %>% 
  relocate(Sample, 
           .after = Time)


# AP0201 <- read.FCS(filename = "data/_raw/CD8_gated_files/export_AP0201_CD8+.fcs", 
#                    transformation = FALSE, 
#                    truncate_max_range = FALSE) %>% 
#   exprs() %>%  
#   as_tibble() %>%
#   sample_n(4000) %>% 
#   mutate(name = "AP0201")
# 
# 
# AP0301 <- flowCore::read.FCS(filename = "data/_raw/CD8_gated_files/export_AP0301_CD8+.fcs", 
#                              transformation = FALSE, 
#                              truncate_max_range = FALSE) %>% 
#   flowCore::exprs() %>% 
#   as_tibble()%>% 
#   sample_n(4000) %>% 
#   mutate(name = "AP0301")
# 
# BC10 <- flowCore::read.FCS(filename = "data/_raw/CD8_gated_files/export_BC10_CD8+.fcs", 
#                            transformation = FALSE, 
#                            truncate_max_range = FALSE) %>% 
#   flowCore::exprs() %>% 
#   as_tibble() %>% 
#   sample_n(4000) %>% 
#   mutate(name = "BC10")
# 
# 
# covid_data <- bind_rows(list( AP0301, AP0201, BC10))


#-------------------------------------

flow_info <- covid_data %>% 
  colnames() %>% 
  as_tibble() %>% 
  rename(Fluor = value)



flow_info_target <- read_xlsx("data/_raw/flow_information_covid.xlsx" )

target_names <- flow_info %>% 
  left_join(flow_info_target, by = "Fluor") %>% 
  mutate(target_new = case_when(is.na(Target)==TRUE ~ Fluor,  
                                !is.na(Target) ~ Target )) %>% 
  pluck("target_new")


colnames(covid_data) <- target_names


asinh_scale <- 150


scale_function_test <- function(x){
  asinh(x/asinh_scale)
}

covid_data1 <- covid_data %>% 
  mutate_at(vars(1:20),scale_function_test )


# UMAP --------------------------------------------------------------------


covid_data_umap <- covid_data %>% 
  select(1:20) %>% 
  as.matrix()

seed <- 1234
covid_data_umap <- covid_data_umap %>% 
  umap()


covid_data_umap <- covid_data_umap %>% 
  as_tibble() %>% 
  mutate(UMAP_1 = V1,
         UMAP_2 = V2, 
         .keep = "unused")


covid_data_umap <-  bind_cols(covid_data_umap, covid_data)


covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2,colour=PD1)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  facet_wrap(~Sample)+
  theme_bw()+
  scale_color_viridis_c(option = "turbo") 


covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_bin2d(bins =100) + 
  coord_fixed(ratio = 1) +
  facet_wrap(~Sample)+
  theme_bw()

asi
covid_data_umap %>%  
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_hex(bins = 50)+
  coord_fixed(ratio = 1) +
  facet_wrap(~Sample)+
  theme_bw()


covid_data_umap_long <- covid_data_umap %>% 
  pivot_longer(cols = c(7:22), 
               names_to = "marker", 
               values_to = "expression")


covid_data_umap_long %>% 
  filter(Sample != "BC10") %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             color = expression))+
  geom_point(size = 0.5, alpha = 0.5)+
  facet_wrap(~ marker)+
  scale_color_viridis_c(option = "turbo") 














covid_plot_umap %>% 
  ggplot(aes(x = UMAP_1, y = UMAP_2)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) + 
  facet_wrap(~) +
  ggtitle("UMAP FlowSOM clustering") + 
  theme_bw()+
  scale_color_viridis_c(option = "turbo") 



plot_UMAP_marker1 <- function(sampleID, marker){
  covid_plot_umap %>% 
    filter(name == sampleID) %>% 
    ggplot(aes(x = UMAP_1,
               y = UMAP_2,))+
    geom_point(aes_string(color = marker),
               size = 0.5)+
    coord_fixed(ratio = 1)+
    scale_color_viridis_c(option = "turbo")+
    theme_bw()}

plot_UMAP_marker1(sampleID = "AP0301", marker = "PD1")
