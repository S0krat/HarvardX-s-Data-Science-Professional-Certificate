library(tidyverse)
library(broom)
library(Lahman)
data("Teams")

dat <- Teams %>% filter(yearID==1971) %>% 
  lm(R ~ BB + HR, data=.) %>%
  tidy(conf.int=TRUE)

Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(lm(R ~ BB + HR, data=.) %>%
    tidy(conf.int=TRUE) %>% 
    filter(term=="BB")) %>%
  ungroup() %>%
  ggplot(aes(x=yearID, y=estimate)) +
  geom_point() +
  geom_smooth(method="lm")

Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(lm(R ~ BB + HR, data=.) %>%
       tidy(conf.int=TRUE) %>% 
       filter(term=="BB")) %>%
  ungroup() %>%
  lm(estimate ~ yearID, data=.) %>%
  tidy()
