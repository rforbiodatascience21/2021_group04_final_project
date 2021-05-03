## here doing the easy version, bu building a function 
## generating dotplot + boxplot

## loading libraries -------------------------------
library(tidyverse)
library(dplyr)
library(ggpubr)
library(stringr)

##Creating a function---------------------------------
# parameters 1: dataframe
#            2: Parent_population 
#            3: Last_population

plot_func <- function(data, Parent_pop, Last_pop, title){
  require(ggpubr)
  Title <- title
  
  output <- data %>%
    filter(Parent_population == Parent_pop,
           Last_population == Last_pop) %>% 
    ggplot(mapping = aes(
      x = cohort_type,
      y = Fraction))+
    geom_boxplot(outlier.shape = NA,
                 fill = NA,
                 color = "gray")+
    stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 fill = "red")+
    stat_compare_means(method = "wilcox.test",
                       alternative = "two.sided",
                       paired = FALSE,
                       conf.level = 0.95,
                       conf.int = TRUE,
                       na.action = na.exclude,
                       test.args = list(exact = FALSE),
                       comparisons = 
                         list(c("HD-1", "HD-2"),
                              c("HD-2", "Patient"),
                              c("HD-1", "Patient")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                          symbols = c("****", "***", "**", "*", "ns"))
                       )+
    theme_classic()+
    labs(title = Title)+
    xlab("Cohort")+
    ylab("% Frequency")
  return(output)
  }

## Testing function-------------------------------------------------

plot_func(covid_data_augment,
          "SARS_multimer+",
          "HLA-DR",
          "Comparing SARS_multimer+ CD38+ cells")


plot_func(covid_data_augment, "SARS_multimer+",
          "CD45RA+_CCR7+",
          "Naive")  

plot_func(covid_data_augment, "SARS_multimer+", "CD38", "CD38 fraction")



##Building an ifElse function-----------------
arg <- FALSE

if(arg == FALSE){
covid_data_augment %>% 
    #filter dataset
  filter(Hospital_status == "Outpatient"
         |
           Hospital_status == "Hospitalized")%>% 
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38") %>% 
    #create ggplot
  ggplot(aes(
    x = Hospital_status,
    y = Fraction))+
  geom_boxplot()
}else{
  print("else")
}

#-------FUNCTIONAL ifElse Function ---------------------

# Creating a function which takes 5 arguments
# 1. Data - df containing groups, gating populations & fractions
# 2. Parent_population - from which the fractions are calculated
# 3. Last_population - the populations of interest
# 4. title - string to create a plot title
# 5. hospital_status - boolean to change function to plot using hospital status instead
#    FALSE = default

# Creating the function
comb_func2 <- function(data,
                       x_axis = cohort_type,  
                       Parent_pop, Last_pop, 
                       title, Hosp_stat = FALSE){
    require(ggpubr)

  #ifElse function reliable on 5th argument  
    output1 <-
    if (Hosp_stat == TRUE) {
      data %>%
        filter(Parent_population == Parent_pop,
               Last_population == Last_pop) %>%
        ggplot(mapping = aes(x = aes_string(x_axis,
                             y = 'Fraction'),
                             na)) +
        geom_boxplot(outlier.shape = NA,
                     fill = NA,
                     color = "gray") +
        geom_dotplot(binaxis = "y",
                     stackdir = "center",
                     fill = "red",
                     binwidth = 0.5,
                     ) +
        stat_compare_means(method = "wilcox.test",
                           test.args = list(exact = FALSE),
                           comparisons = list(c("HD-1", "HD-2"),
                                              c("HD-2", "Patient"),
                                              c("HD-1", "Patient")),
                           symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("****", "***", "**", "*", "ns")))+
        theme_classic()+
        labs(title = title) +
        xlab("Cohort") +
        ylab("% Frequency")
    }else{
      data %>%
        filter(Parent_population == Parent_pop,
               Last_population == Last_pop) %>%
        ggplot(data,
               mapping = aes(x = Hospital_status,
                             y = Fraction)) +
        geom_boxplot(outlier.shape = NA,
                     fill = NA,
                     color = "gray") +
        geom_dotplot(binaxis = "y",
                     stackdir = "center",
                     fill = "red") +
  #      geom_signif(comparisons = list(c("Outpatient", "Hospitalized" )),
   #               test = wilcox.test,
    #              na.rm = T,
     #             test.args = list(exact = FALSE)) +
        stat_compare_means(method = "wilcox.test",
                       #    data = data %>%  filter(Parent_population == Parent_pop,
                        #                      Last_population == Last_pop) %>% na.omit(), 
                           na.rm = T,
                           comparisons = list(c("Outpatient", "Hospitalized")))+ 
                       #    symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1)), 
                        #                      symbols = c("****", "***", "**", "*", "ns")))+
        theme_classic() +
        labs(title = title) +
        xlab("Hospital Status") +
        ylab("% Frequency")
    }
    return(output1)
  }

wilcox.test(covid_data_augment$Fraction ~ covid_data_augment$Hospital_status, 
            data = covid_data_augment %>% filter(Parent_population == "SARS_multimer+",Last_population == "CD38"))


my_df <- covid_data_augment %>% 
  filter(Parent_population == "SARS_multimer+",Last_population == "CD38") %>% na.omit()
table(my_df$Hospital_status)


#----------new function withoput ifElse as main devider)-----------

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


new_func(data = covid_data_augment,
           x_axis = "Hospital_status",
           Parent_pop = "SARS_multimer+",
           Last_pop = "CD38",
           title = "test_title")




covid_data_augment %>%
  filter(Parent_population == "SARS_multimer+",
         Last_population == "CD38) %>% 
  ggplot(mapping = aes(
    x = cohort_type,
    y = Fraction))+
  geom_boxplot(outlier.shape = NA,
               fill = NA,
               color = "gray")+
  stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               fill = "red")+
  stat_compare_means(method = "wilcox.test",
                     alternative = "two.sided",
                     paired = FALSE,
                     conf.level = 0.95,
                     conf.int = TRUE,
                     na.action = na.exclude,
                     test.args = list(exact = FALSE),
                     comparisons = 
                       list(c("HD-1", "HD-2"),
                            c("HD-2", "Patient"),
                            c("HD-1", "Patient")),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))
  )+
  theme_classic()+
  labs(title = Title)+
  xlab("Cohort")+
  ylab("% Frequency")

