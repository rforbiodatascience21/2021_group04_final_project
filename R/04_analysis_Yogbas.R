# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggpubr")
library("ggsignif")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
covid_data_augment <- read_tsv(file = "data/03_covid_data_augment.tsv")


# Wrangle data ------------------------------------------------------------
#my_data_clean_aug %>% ...


# Model data
#my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

#Expression of cell surface markers in patient - Violin plot along with box plot
#Most of the codes are a copy of Marie's code for boxplot generation

covid_data_augment %>% 
  filter(Last_population == c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
         str_detect(Parent_population, "SARS")) %>%
  ggplot(aes(x = cohort_type,
             y = Fraction,
             color = cohort_type))+
  geom_point(position=position_jitterdodge(dodge.width=0.9))+
  geom_boxplot(width=.1, outlier.colour=NA, outlier.shape = NA)+
  facet_wrap(vars(Last_population),
             nrow = 1)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()
  )+
  scale_y_continuous(limits=c(1,105))+
  labs(title = "Expression of cell surface markers",
       y = "% of SARS-CoV-2 mult+ CD8+ T cells")+
  geom_signif(comparisons = list(c("HD-1", "Patient"),
                                 c("HD-2", "Patient")),
              method = "kruskal.test",
              map_signif_level = TRUE,
              margin_top = 0.005,
              step_increase = 0.1,
              vjust = 0.7,
              textsize = 3)


#Plotting a correlation plot for the cell surface markers detected in 
#hospitalized vs outpatients
#The plot is not working, difficulty in plotting points for hospitalized vs outpatient


covid_data_augment_wide <- covid_data_augment %>% 
  pivot_wider(Last_population,
              names_from = Hospital_status,
                            values_from = Fraction)

covid_data_augment_wide %>% 
  #filter(Last_population == c("CD27", "CD38", "CD39", "CD57", "CD69", "HLA-DR", "PD-1"),
   #      str_detect(Parent_population, "SARS")) %>% 
  ggplot(mapping = aes(x = "Hospitalized", 
                y = "Outpatient"
                ))+
  geom_point()+
  facet_wrap(vars(Last_population))



##Function for facet wrap
#Function that works taken from Marcus - Single graph function

plot_func <- function(data, Parent_pop, Last_pop, title)
  {
  require(ggpubr)
  Title <- title
  
  output <- data %>%
    filter(Parent_population == Parent_pop,
           Last_population == Last_pop) %>% 
    ggplot(mapping = aes(
      x = cohort_type,
      y = Fraction,
      color = cohort_type))+
    geom_boxplot(outlier.shape = NA,
                 fill = NA,
                 color = "gray")+
    stat_boxplot(geom='errorbar', linetype=1, width=0.2)+
    geom_dotplot(binaxis = "y",
                 stackdir = "center",
                 fill = "red")+
    facet_wrap(vars(Last_pop))
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


#--------RUNNING THE TEST FUNCTION--------
plot_func(covid_data_augment,
          "SARS_multimer+",
          "HLA-DR",
          "Comparing SARS_multimer+ CD38+ cells")


# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)