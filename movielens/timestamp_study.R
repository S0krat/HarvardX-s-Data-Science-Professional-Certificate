library(tidyverse)
library(dslabs)
library(caret)
library(lubridate)

edx <- read_rds("edx.Rds")
edx <- movielens

edx <- edx %>% select(rating, timestamp) %>%
  mutate(date=as_datetime(timestamp)) %>% 
  select(-timestamp)

# dependence of rating on year
edx %>% mutate(date=as.factor(year(date))) %>%
  group_by(date) %>%
  summarise(mean_rat = mean(rating)) %>% 
  ggplot(aes(x=date, y=mean_rat, group=1)) +
  geom_line()

# dependence of rating on week
edx %>% mutate(date=as.factor(week(date))) %>%
  group_by(date) %>%
  summarise(mean_rat = mean(rating)) %>% 
  ggplot(aes(x=date, y=mean_rat, group=1)) +
  geom_line()

# dependence of rating on week day
edx %>% mutate(date=as.factor(wday(date))) %>%
  group_by(date) %>%
  summarise(mean_rat = mean(rating)) %>% 
  ggplot(aes(x=date, y=mean_rat, group=1)) +
  geom_line()

# dependence of rating on hour
edx %>% mutate(date=as.factor(hour(date))) %>%
  group_by(date) %>%
  summarise(mean_rat = mean(rating)) %>% 
  ggplot(aes(x=date, y=mean_rat, group=1)) +
  geom_line()
