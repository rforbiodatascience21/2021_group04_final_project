if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")


BiocManager::install("flowCore")
BiocManager::install("FlowSOM")

install.packages("Rtsne")
install.packages("tidyverse")
install.packages("uwot")
install.packages("openxlsx")


library(flowCore)
library(FlowSOM)
library(Rtsne)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(uwot)

#############################
### LOAD AND PREPARE DATA ###
#############################
# load data concatenated data from FlowJo

files <- list.files('data/_raw/CD8_gated_files')


data_FC <- data.frame()

for (f in files) {
  name= str_split(f, "_")[[1]][2]
  data_FC_sample <- flowCore::exprs(flowCore::read.FCS(filename = paste0("data/_raw/CD8_gated_files/",f), 
                                                       transformation = FALSE, 
                                                       truncate_max_range = FALSE))
  data_FC_sample <- as.data.frame(data_FC_sample)
  data_FC_sample$name <- name 
  data_FC  <- rbind(data_FC ,data_FC_sample)
}


#data_FC <- flowCore::exprs(flowCore::read.FCS(filename = "191210_Screen 1/191210_1_1233_A_0_0_001.fcs" ,transformation = FALSE, truncate_max_range = FALSE))
head(data_FC)
dim(data_FC)
#kan dette laves til tidyverse
class(data_FC$Time)



# downsample 
head(data_FC)

flow_info <- data_FC %>%
  colnames() %>% 
  as.data.frame()


flow_info <- flow_info %>% 
  mutate(Fluor = colnames(data_FC)) %>% 
  select(Fluor)


flow_info_target <- read.xlsx("data/_raw/flow_information_covid.xlsx" )

flow_info <- flow_info %>% 
  left_join(flow_info_target, by = "Fluor") %>% 
  mutate(target_new = case_when(is.na(Target)==TRUE ~ Fluor,  
                                !is.na(Target) ~ Target ))

#fluor_names <- pull(flow_info, Fluor)
target_names <- pull(flow_info, target_new)


### skal laves om til tidyverse uden $

#my_col <- setNames(flow_info$target_new,
#                   flow_info$Fluor)

# ændre data column names
colnames(data_FC) <- target_names

  
#colnames(data_FC) <- my_col[colnames(data_FC)]


# only if you have many cells and its slow 
#data_FC_downsample <- data_FC %>% 
#   group_by(name) %>% 
#   sample_n(407)

head(data_FC)
# select protein marker columns to use for clustering
marker_cols <- c(5:9,11,13,15,17:20)

# apply arcsinh transformation
# (with standard scale factor of 5 for CyTOF data; alternatively 150 for flow 
# cytometry data; see Bendall et al. 2011, Science, Supplementary Figure S2)
asinh_scale <- 150


#NB selecting markers
#data_FC <- data_FC %>% 
#  select(marker_cols)

#data_FC <- asinh(data_FC/asinh_scale)

data_FC[,1:20] <- asinh(data_FC[,1:20]/asinh_scale)



# create flowFrame object (required input format for FlowSOM)
data_FlowSOM <- flowCore::flowFrame(as.matrix(data_FC[,1:20]))
data_FC_new <- NULL
data_FC_td <- NULL
data_FC_sample <- NULL


# set seed for reproducibility
set.seed(1234)


# run FlowSOM (initial steps prior to meta-clustering)
out <- FlowSOM::ReadInput(data_FlowSOM, transform = FALSE, scale = FALSE)
out <- FlowSOM::BuildSOM(out, colsToUse = marker_cols)
out <- FlowSOM::BuildMST(out)


# optional visualization
# observing that columns about cell size has most influence, might be removed!
FlowSOM::PlotStars(out)


#extract cluster labels (pre meta-clustering) from output object
labels_pre <- out$map$mapping[, 1]


# specify final number of clusters for meta-clustering (can also be selected 
# automatically, but this often does not perform well)
k <- 8


# run meta-clustering

# note: In the current version of FlowSOM, the meta-clustering function 
# FlowSOM::metaClustering_consensus() does not pass along the seed argument 
# correctly, so results are not reproducible. We use the internal function 
# ConsensusClusterPlus::ConsensusClusterPlus() to get around this. However, this
# will be fixed in the next update of FlowSOM (version 1.5); then the following 
# (simpler) code can be used instead:
#seed <- 1234
#out <- FlowSOM::metaClustering_consensus(out$map$codes, k = k, seed = seed)
seed <- 1234
out <- ConsensusClusterPlus::ConsensusClusterPlus(t(out$map$codes), maxK = k, seed = seed)
out <- out[[k]]$consensusClass


# extract cluster labels from output object
labels <- out[labels_pre]


# summary of cluster sizes and number of clusters
table(labels)
length(table(labels))



#######################
###### Run UMAP #######
#######################

library(uwot)

labels_plot <- labels

unique(data_FC$name)

# prepare data for umapr (matrix format required)
data_umap <- data_FC[,1:20]
data_umap <- as.matrix(data_umap)
dups <- duplicated(data_umap)
# no duplictaes :)
#data_umap <- data_umap[!dups, ]


umap_emb <- umap(data_umap)
?umap

# prepare umap embedding output data for plot
data_plot_umap <- as.data.frame(umap_emb)
colnames(data_plot_umap) <- c("UMAP_1", "UMAP_2")
head(data_plot_umap);dim(data_plot_umap)

data_plot_umap[,"cluster"] <- as.factor(labels_plot)
data_plot_umap <- cbind(data_plot_umap,as.data.frame(data_umap))
data_plot_umap[,"sample"] <- as.factor(data_FC$name )


unique(data_plot_umap$sample)
head(data_plot_umap);dim(data_plot_umap)


# plot 2-dimensional umap projection
#coloured after cluster
ggplot(data_plot_umap, aes(x = UMAP_1, y = UMAP_2,colour=cluster)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) + 
  facet_wrap(.~sample) +
  ggtitle("UMAP FlowSOM clustering") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))+
  guides(color = guide_legend(override.aes = list(size=5)))




ggplot(data_plot_umap, aes(x = UMAP_1, y = UMAP_2,colour=CD45RA)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) + 
  facet_wrap(.~sample) +
  ggtitle("UMAP FlowSOM clustering") + 
  theme_bw()+
  scale_color_viridis_c(option = "inferno") 
# theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),
#                  legend.position="bottom",legend.text = element_text(size=20),legend.title = element_text(size=20))+
# guides(color = guide_legend(override.aes = list(size=5)))
#save plot in work directory
#ggsave("FlowSOM_UMAP_k10_plot.pdf", height = 8, width = 8)

data_plot_umap %>% 
  ggplot(., aes(x = `SSC-A`, y = `FSC-A`)) +
  geom_point()


colnames(data_plot_umap)

class(data_plot_umap)
plot_density <- function(marker) {
  data_plot_umap %>% 
    ggplot(., aes_string(x = marker)) + 
    geom_histogram(aes(fill = cluster), position = "identity", binwidth = 0.1, alpha = 0.5) +
    theme_bw() }





data_plot_umap
plot_density("PD1")
plot_density("CD57")




for (col in colnames(data_plot_umap)) {
  print(col)
  plot_density(col)
}


ggplot(data_plot_umap, aes(x = UMAP_1, y = UMAP_2,colour=CD57)) + 
  scale_color_gradient(low = "grey",high = "red") +
  geom_point(size = 1) + 
  coord_fixed(ratio = 1) + 
  ggtitle("UMAP projection with FlowSOM clustering") + 
  theme_bw()

# Signes gates -------

data_plot_umap


#UMAP plot with different markers

plot_UMAP_marker1 <- function(sampleID, marker){
  data_plot_umap %>% 
    filter(sample == sampleID) %>% 
    ggplot(aes(x = UMAP_1,
               y = UMAP_2,))+
    geom_point(aes_string(color = marker),
               size = 0.5)+
    coord_fixed(ratio = 1)+
    scale_color_viridis_c(option = "turbo")+
    theme_bw()}

plot_UMAP_marker1(sampleID = "AP0301", marker = "PD1")

data_plot_umap %>% 
  ggplot(aes(x = UMAP_1, 
             y = UMAP_2,
             colour=cluster)) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) + 
  facet_wrap(.~sample) +
  ggtitle("UMAP FlowSOM clustering") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))+
  guides(color = guide_legend(override.aes = list(size=5)))



#density plots

plot_density1 <- function(marker) {
  data_plot_umap %>% 
    ggplot(., aes_string(x = marker)) + 
    geom_density(aes(color = cluster), position = "identity") +
    theme_bw() }

plot_density1("CD57")



#wrangling data_plot_umap

data_plot_umap_long <- data_plot_umap %>% 
  select(-c("FSC-A","FSC-H", "SSC-A", "SSC-H", "CD8","CD4", 
            "CD3", "Live-Dead","UMAP_1", "UMAP_2")) %>% 
  pivot_longer(cols = c("Multimer-APC":"CD57"), names_to = "target", values_to = "expr")


data_plot_umap_sum <- data_plot_umap_long %>% 
  group_by(cluster, target, sample) %>% 
  summarise(mean(expr), 
            median(expr),
            sd(expr)) %>% 
  mutate(mean_expr =`mean(expr)`,
         median_expr = `median(expr)`,
         SD_expr = `sd(expr)`) %>% 
  select(-c("mean(expr)":"sd(expr)"))



# heatmap of markers for clusters

data_plot_umap_sum %>% 
  filter(sample =="AP0301") %>% 
  ggplot(aes(x = target,
             y = cluster,
             fill = mean_expr))+
  geom_tile()+
  scale_fill_viridis_c(option = "plasma")



#exploring clusters troughout markers
data_plot_umap_sum %>% 
  filter(sample =="AP0301",
         cluster %in% c(2:3)) %>% 
  ggplot(aes(x = target,
             y = mean_expr,
             color = cluster,
             group = cluster))+
  geom_point()+
  geom_line()+
  theme_bw()

  
data_plot_umap_sum %>% 
  filter(sample =="AP0301") %>% 
  ggplot(aes(x = cluster,
             y = mean_expr,
             fill = target))+
  geom_col(position = position_dodge(0.9))



#normalisation of values of markers

min_expression <- data_plot_umap_long %>% 
  group_by(cluster, target, sample) %>% 
  summarise(min(expr)) %>% 
  mutate(min_expr = `min(expr)`) %>% 
  select(-`min(expr)`)

data_plot_umap_long <- data_plot_umap_long %>% 
  left_join(min_expression, by = c("cluster", "target", "sample"))

