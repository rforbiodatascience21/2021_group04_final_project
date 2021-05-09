# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
fcs_covid_data_clean <- read_tsv(file = "data/02_fcs_covid_data_clean.tsv")


flow_info_target <- read_tsv(file = "data/01_fcs_flow_info_target.tsv")

# Wrangle data ------------------------------------------------------------

flow_info <- fcs_covid_data_clean %>% 
  colnames() %>% 
  as_tibble() %>% 
  rename(Fluor = value)

target_names <- flow_info %>% 
  left_join(flow_info_target, by = "Fluor") %>% 
  mutate(target_new = case_when(is.na(Target)==TRUE ~ Fluor,  
                                !is.na(Target) ~ Target )) %>% 
  pluck("target_new")

colnames(fcs_covid_data_clean) <- target_names



# should probaly be placed in function script
asinh_scale <- 150

scale_function <- function(x){
  asinh(x/asinh_scale)
}

fcs_covid_data_aug <- fcs_covid_data_clean %>% 
  mutate_at(vars(1:20),scale_function)


# Write data --------------------------------------------------------------
write_tsv(x = fcs_covid_data_aug,
          file = "data/03_fcs_covid_data_aug.tsv")
