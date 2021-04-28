## creating plots to compare 
## firstly: patient samples vs HD-1, and HD-2.
## Secondly: Hospitlized Patients vs outPatients.

## loading libraries -------------------------------
library(tidyverse)
library(dplyr)
library(ggpubr)

## idea is to create a function, which will generate graph/graphs
## depending what input I want. 
## so lets say I have the full dataframe, I filter on CD38, 
## put that thtough the function, and I'll plot the CD38 data,
## grouped on cohort_type
##EXAMPLE:::
#data_summary <- function(data, varname, groupnames){
 # require(plyr)
  #summary_func <- function(x, col){
   # c(mean = mean(x[[col]], na.rm=TRUE),
    #  sd = sd(x[[col]], na.rm=TRUE))
  #}
  #data_sum<-ddply(data, groupnames, .fun=summary_func,
  #                varname)
#  data_sum <- rename(data_sum, c("mean" = varname))
 # return(data_sum)
  
##------------------------

##testing to nest data, then do sumfunctions
test_nested <- 
  covid_data_augment %>% 
  group_by(cohort_type,
           Hospital_status,
           Parent_population,
           Last_population,
           Gating,
           T_cell
           ) %>% 
  nest() %>% 
  ungroup()


## summarize data (mean, sd)
 input_sum <- covid_data_augment %>% 
  group_by(cohort_type,
           Parent_population,
           Last_population) %>%
   summarise(mean = mean(Fraction, na.rm = TRUE),
            sd = sd(Fraction, na.rm = TRUE),
            min = min(Fraction, na.rm = TRUE),
            max = max(Fraction, na.rm = TRUE))
   


covid_data_augment_TEST %>% 
  filter(Parent_population == "SARS_multimer",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 fill = "red")+
  geom_boxplot(outlier.shape = NA,
               fill = NA,
               color = "gray")+
  stat_compare_means(method = "wilcox.test", comparisons = 
                       list(c("HD-1", "HD-2"),
                            c("HD-2", "Patient"),
                            c("HD-1", "Patient")),
                     hide.ns = TRUE)+
  theme_classic()
  

#--------
p1 +  input_sum %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(aes(x = cohort_type,
             y = sd,
             ymin = min,
             ymax = max))+
  geom_errorbar()

p1+p2

  #--------------------------------
  Tst.DF = covid_data_augment %>% 
  mutate(Fraction2 = replace_na(Fraction, 0)) %>% 
  group_by(Parent_population, cohort_type) %>%
  summarize(mean_flow = mean(Fraction2), SD_flow = sd(Fraction2))
  

covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               fill = "gray")+
  geom_errorbar(aes(ymin = min(Fraction), ymax = max(Fraction), color = "red"))+
  theme_classic()+
  theme(legend.position = "none")

## Violin plot -------------------------------------------
covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
geom_violin()+
  +
  theme_classic()

## Functional dotplot------------------------------------
Tst.DF = covid_data_augment %>% 
  mutate(Fraction2 = replace_na(Fraction, 0)) %>% 
  group_by(Parent_population, cohort_type) %>%
  summarize(mean_flow = mean(Fraction2), SD_flow = sd(Fraction2))

covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               fill = "gray")+
  geom_boxplot(outlier.shape = NA,
               fill = NA)+
  theme_classic()+
  theme(legend.position = "none")

## functional boxplot ----------------------------------
require(ggpubr)
covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_boxplot(outlier.shape = NA,
               color = "gray",
               fill = NA)+
  geom_jitter(color = "red")+
  theme_classic()+
  stat_compare_means(method = "wilcox.test",
                     comparisons = list(c("HD-1", "HD-2"),
                                        c("HD-2", "Patient"),
                                        c("HD-1", "Patient")),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))
