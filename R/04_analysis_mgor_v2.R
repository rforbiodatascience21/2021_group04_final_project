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

## Testing function-------------------------------------------------

plot_func(covid_data_augment,
          "SARS_multimer+",
          "HLA-DR",
          "Comparing SARS_multimer+ CD38+ cells")


plot_func(covid_data_augment, "SARS_multimer+", "CD45RA+_CCR7+", "Naive")  



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
comb_func2 <- function(data,Parent_pop, Last_pop, title, Hosp_stat = FALSE){
    require(ggpubr)

  #ifElse function reliable on 5th argument  
    output1 <-
    if (Hosp_stat == TRUE) {
      data %>%
        filter(Parent_population == Parent_pop,
               Last_population == Last_pop) %>%
        ggplot(mapping = aes(x = cohort_type,
                             y = Fraction,
                             na)) +
        geom_boxplot(outlier.shape = NA,
                     fill = NA,
                     color = "gray") +
        geom_dotplot(binaxis = "y",
                     stackdir = "center",
                     fill = "red") +
        stat_compare_means(
          method = "t.test",
          alternative = "two.sided",
          paired = FALSE,
          conf.level = 0.95,
          conf.int = TRUE,
          na.action = na.exclude,
          comparisons =
            list(
              c("HD-1, HD-2"),
              c("HD-2, Patient"),
              c("HD-1, Patient")),
          symnum.args = list(
            cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
            symbols = c("****", "***", "**", "*", "ns")
            ),
            bracket.size = .6,
            size = 4
        )+
        theme_classic() +
        labs(title = title) +
        xlab("Cohort") +
        ylab("% Frequency")
    }else{
      data %>%
        filter(Parent_population == Parent_pop,
               Last_population == Last_pop) %>%
        ggplot(data,
               mapping = aes(x = cohort_type,
                             y = Fraction)) +
        geom_boxplot(outlier.shape = NA,
                     fill = NA,
                     color = "gray") +
        geom_dotplot(binaxis = "y",
                     stackdir = "center",
                     fill = "red") +
        stat_compare_means(
          method = "t.test",
          alternative = "two.sided",
          paired = FALSE,
          conf.level = 0.95,
          conf.int = TRUE,
          na.action = na.exclude,
          comparisons = list(c("Hospitalized, Outpatient")),
          symnum.args = list(
            cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
            symbols = c("****", "***", "**", "*", "ns")
          ),
          bracket.size = .6,
          size = 4
        )+
        theme_classic() +
        labs(title = title) +
        xlab("Hospital Status") +
        ylab("% Frequency")
    }
    return(output1)
  }

comb_func2(covid_data_augment,
           Parent_pop = "SARS_multimer+",
           Last_pop = "CD38",
           title = "test_title",
           FALSE)

#får error message: Computation failed in `stat_signif()`:
#  missing value where TRUE/FALSE needed
# 
# kolla länk om där är något jag kan göra
#
#https://cran.r-project.org/web/packages/ggsignif/ggsignif.pdf
