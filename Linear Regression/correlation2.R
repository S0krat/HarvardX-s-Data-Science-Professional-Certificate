library(tidyverse)
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

mothers_mean <- mean(female_heights$mother)
mothers_sd <- sd(female_heights$mother)
daughters_mean <- mean(female_heights$daughter)
daughters_sd <- sd(female_heights$daughter)
mothers_daughters_cor <- cor(female_heights$mother, female_heights$daughter)

slope <- mothers_daughters_cor * daughters_sd / mothers_sd
intercept <- daughters_mean - mothers_mean * slope

height_predict <- function(mother_height) {
  intercept + mother_height * slope
}
