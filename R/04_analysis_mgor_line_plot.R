#---- code for geom_line -----
# has to be cleaned up to look nice.

#---- this does a facetwrap, of selected group, however, has to figure out how to clean it from graphs which don't has CEF expression

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

#----- single plot 

covid_data_augment %>% 
  filter(
    cohort_type == "Patient",
    str_detect(Parent_population, "multimer+"),
    Last_population == "CD38") %>%
  ggplot(
    aes(
      x = Parent_population,
      y = Fraction))+
  geom_point(color = "dimgray")+
  geom_line(aes(group = SampleID),
            color = "dimgray")+
  theme(legend.position = "none")
