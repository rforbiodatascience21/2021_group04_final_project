## loading libraries -------------------------------
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)

##Creating a function---------------------------------
# parameters 1: dataframe
#            2: Parent_population 
#            3: Last_population
#            4: Title


new_func <- function(data,
                     x_axis,  
                     Parent_pop, Last_pop, 
                     title){
  require(ggpubr)
  
  subtitle <- c(Parent_pop, Last_pop)
  
  comp_list_Cohort_type <- list(c("HD-1", "HD-2"),
                                c("HD-2", "Patient"),
                                c("HD-1", "Patient"))
  
  comp_list_Hospital_status <- list(c("Outpatient", "Hospitalized"))
  
  output1 <-
    data %>%
    filter(Parent_population == Parent_pop,
           Last_population == Last_pop) %>%
    na.omit() %>%
    ggplot(mapping = aes_string(
      x = x_axis,
      y= 'Fraction') +
        geom_boxplot(outlier.shape = NA,
                     fill = NA,
                     color = "gray") +
        scale_y_continuous()+
        geom_dotplot(binaxis = "y",
                     stackdir = "center",
                     fill = "red")+
        
        # stat_compare_means(
        #   mapping = aes_string(
        #   x = x_axis,
        #   y= 'Fraction'),
        #   method = "wilcox.test",
        #                    test.args = list(exact = FALSE),
        #                    if(x_axis=="cohort_type"){
        #                      comparisons = comp_list_Cohort_type}
        #                    else {comparisons = comp_list_Hospital_status},
        #                    symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      #                                       symbols = c("****", "***", "**", "*", "ns")))+
      theme_classic()+
        labs(title = title, 
             subtitle = subtitle,
             y =)
      return(output1)
}

#---CALLING THE FUNCTION-------------


new_func(data = covid_data_augment,
         x_axis = "Hospital_status",
         Parent_pop = "SARS_multimer+",
         Last_pop = "CD38",
         title = "test_title")




#----------EXAMPLE FUNCTION WHICH WORKS----------
plot_func <- function(data, Parent_pop, Last_pop, strat, title){
  require(ggpubr)
  Title <- title
  output <- data %>%
    filter(Parent_population == Parent_pop,
           Last_population == Last_pop) %>% 
    ggplot(mapping = aes(y = Fraction,
                         x = Last_pop,
                         color = strat))+
    geom_boxplot(outlier.shape = NA,
                 fill = NA,
                 color = "gray")+
    stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
    geom_point()+
    theme_classic()+
    labs(title = Title)+
    xlab("Cohort")+
    ylab("% of")
  return(output)
}

#--------RUNNING THE TEST FUNCTION--------
plot_func(data = covid_data_augment,
          Parent_pop = "SARS_multimer+",
          Last_pop = "HLA-DR",
          strat = cohort_type,
          title = "Comparing SARS-Cov2 multimer+ cells")

