# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Load data ---------------------------------------------------------------
covid_data_raw <- read_excel("data/_raw/covid_data.xlsx")


# Write data --------------------------------------------------------------
write_tsv(x = covid_data_raw,
          file = "data/01_covid_data.tsv")

