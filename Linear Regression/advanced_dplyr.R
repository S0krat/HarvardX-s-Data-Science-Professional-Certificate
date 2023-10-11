library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

obs_num <- galton %>% group_by(pair) %>% summarise(n = n())
obs_num

obs_cor <- galton %>% group_by(pair) %>% summarise(c = cor(childHeight, parentHeight))
obs_cor

library(broom)
dat <- galton %>% group_by(pair) %>% 
  summarise(tidy(lm(childHeight~parentHeight), conf.int=TRUE)) %>%
  filter(term=="parentHeight")
