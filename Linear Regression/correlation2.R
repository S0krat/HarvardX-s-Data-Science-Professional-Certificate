library(tidyverse)
set.seed(1989)
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)