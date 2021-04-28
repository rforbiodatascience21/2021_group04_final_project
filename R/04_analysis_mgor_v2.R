## here doing the easy version, bu building a function 
## generating dotplot + boxplot

## loading libraries -------------------------------
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)

#-Creating a function---------------------------------
# parameters 1: dataframe
#            2: Parent_population 
#            3: Last_population

plot_func <- function(data, Parent_pop, Last_pop, title){
  require(ggpubr)
  Title <- title
  
  output <- data %>%
    filter(Parent_population == Parent_pop,
           Last_population == Last_pop) %>% 
    ggplot(data,
           mapping = aes(
      x = cohort_type,
      y = Fraction))+
    geom_boxplot(outlier.shape = NA,
                 fill = NA,
                 color = "gray")+
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 fill = "red")+
    stat_compare_means(method = "wilcox.test",
                       alternative = "two.sided",
                       paired = FALSE,
                       conf.level = 0.95,
                       conf.int = TRUE,
                       na.action = na.exclude,
                       comparisons = 
                         list(c("HD-1", "HD-2"),
                              c("HD-2", "Patient"),
                              c("HD-1", "Patient")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                          symbols = c("****", "***", "**", "*", "ns")))+
    theme_classic()+
    labs(title = Title)+
    xlab("Cohort")+
    ylab("% Frequency")
  return(output)
  }

## Testing funciton

plot_func(covid_data_augment,
          "SARS_multimer+",
          "HLA-DR",
          "Comparing SARS_multimer+ CD38+ cells")
  
