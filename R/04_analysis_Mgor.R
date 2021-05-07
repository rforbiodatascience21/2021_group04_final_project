## creating plots to compare 
## firstly: patient samples vs HD-1, and HD-2.
## Secondly: Hospitlized Patients vs outPatients.

## loading libraries -------------------------------
library(tidyverse)
library(dplyr)
library(ggpubr)

install.packages("patchwork")
library(patchwork)

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
   


covid_data_augment %>% 
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



#-------------
covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  View()




#---------SIGNES VERSION---------

covid_data_augment %>% 
  filter(cohort_type == "Patient",
         Last_population == c("CD38", "PD1")) %>% 
  ggplot(aes(x = Last_population,
             y = Fraction,
             fill = Parent_population))+
  geom_boxplot(outlier.shape = NA)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               position = position_dodge(0.75))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "% of multimer+ CD8+ T cells",
       fill = "")+
  scale_fill_discrete(labels = c("CEF", "SARS-CoV-2"))


##----Testing to do my own facet_wrap
pops <- c("CD38", "PD1")

covid_data_augment %>% 
  filter(Last_population %in% c("CD27",
                                "CD38",
                                "CD39",
                                "CD57",
                                "CD69",
                                "HLA-DR",
                                "PD-1"),
         str_detect(Parent_population, "SARS")) %>% 
  ggplot(aes(x = cohort_type,
             y = Fraction,
             color = cohort_type))+
  geom_boxplot(outlier.shape = NA)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
              dotsize = 0.3,
              na.rm = TRUE)+
  facet_wrap(vars(Last_population),
             nrow = 1)+
  theme_classic()+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              margin_top = 0.005,
              step_increase = 0.1,
              vjust = 0.7,
              textsize = 3)



covid_data_augment_wider_SARS <- covid_data_augment %>% 
  select(-Gating) %>% 
  filter(Parent_population == "SARS_multimer+") %>% 
  pivot_wider(names_from = Last_population,
              values_from = Fraction)

covid_data_augment_wider_SARS %>% View()


covid_data_augment_wider_SARS %>%
  ggplot(aes(
    x = CD38,
    y = CD39,
    color = cohort_type))+
  geom_point()+
  geom_smooth(method = lm,
              color = "dimgray")+
  theme_classic()

#---- line plot

covid_data_augment %>% 
  filter(
    cohort_type == "Patient",
    str_detect(Parent_population, "multimer+"),
    Last_population == "CD38") %>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction
    ))+
  geom_point()+
  geom_line(aes(group = SampleID))+
  theme(legend.position = "none")

#-----Facet wrapping line plot-----

covid_data_augment %>% 
  filter

covid_data_augment %>% 
  filter(Last_population %in% c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
    cohort_type == "Patient",
    str_detect(Parent_population, "multimer+")) %>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction))+
  geom_point(color = "dimgray")+
  geom_line(aes(group = SampleID),
            color = "dimgray")+
  theme(legend.position = "none")+
 facet_wrap(vars(Last_population),
            scales = "free_y")+
  theme_classic()

#---better version of FacetWrap
covid_data_augment %>% 
  filter(,
         cohort_type == "Patient",
         str_detect(Parent_population, "multimer+")) %>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction))+
  geom_point(color = "dimgray")+
  geom_line(aes(group = SampleID),
            color = "dimgray")+
  theme(legend.position = "none")+
  facet_wrap(vars(Last_population),
             scales = "free_y")+
  theme_classic()



#---- Picking the ones we have data of-----

covid_data_augment %>% 
  filter(
    cohort_type == "Patient",
    str_detect(Parent_population, "multimer+"),
    Last_population == "CD27") %>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction))+
  geom_point(color = "dimgray")+
  geom_line(aes(group = SampleID),
            color = "dimgray")+
  theme(legend.position = "none")
  
