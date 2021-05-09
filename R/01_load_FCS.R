# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------


# package from BioConductor to install to read FCS files: 

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# 
# BiocManager::install("flowCore")


library(flowCore)
library(readxl)
library(tidyverse)
#library(uwot)

# Load data ---------------------------------------------------------------
fcs_covid_files <- list.files(path = "data/_raw/CD8_gated_files/", 
                          pattern = "\\.fcs$",
                          full.names = TRUE)




flow_info_target <- read_xlsx("data/_raw/flow_information_covid.xlsx" )


# Wrangle data ------------------------------------------------------------

fcs_covid_data <- fcs_covid_files %>% 
  set_names() %>% 
  map_dfr(.f = ~read.FCS(filename = .x,
                         transformation = FALSE,
                         truncate_max_range = FALSE) %>% 
            exprs() %>% 
            as_tibble(), 
          .id = "name"
  )


#downsample of tibble

fcs_covid_data <- fcs_covid_data %>% 
  group_by(name) %>% 
  sample_n(1000)





# Write data --------------------------------------------------------------
write_tsv(x = fcs_covid_data,
          file = "data/01_fcs_covid_data.tsv")


write_tsv(x = flow_info_target,
          file = "data/01_fcs_flow_info_target.tsv")
