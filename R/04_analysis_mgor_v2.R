## here doing the easy version, bu building a function 
## generating dotplot + boxplot

## loading libraries -------------------------------
require(tidyverse)
require(dplyr)
require(ggpubr)

covid_data_augment_TEST <- covid_data_augment %>%
  select(Parent_population) %>% 
  str_replace("SARS_Multimer+","+", fix_replacement(""))


#-----------------------
  
covid_data_augment_TEST %>%
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>%
  ggplot(data,
         mapping = aes(
           x = cohort_type,
           y = Fraction))+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               fill = "gray")+
  stat_compare_means(method = "t.test", comparisons = 
                       list(c("HD-1", "HD-2"),
                            c("HD-2", "Patient"),
                            c("HD-1", "Patient")),
                     hide.ns = TRUE,
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                        symbols = c("****", "***", "**", "*", "ns")) 
  )+
  theme_classic()+
  geom_boxplot(outlier.shape = NA,
               fill = NA)

#---------------------------------


plot_func <- function(data, Parent_population, Last_population){
  output <- data %>%
    filter(Parent_population == Parent_population,
           Last_population == Last_population) %>%
    ggplot(data,
           mapping = aes(
      x = cohort_type,
      y = Fraction))+
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 fill = "gray")+
    stat_compare_means(method = "t.test", comparisons = 
                         list(c("HD-1", "HD-2"),
                              c("HD-2", "Patient"),
                              c("HD-1", "Patient")),
                       hide.ns = TRUE,
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                          symbols = c("****", "***", "**", "*", "ns")) 
    )+
    theme_classic()+
    geom_boxplot(outlier.shape = NA,
                 fill = NA)
  return(output)
}

## Testing funciton
plot_func(covid_data_augment_TEST, SARS_multimer+, CD38)
  
  
