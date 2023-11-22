library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movielens %>% group_by(movieId) %>% 
  summarise(n_rat=n(), year=year[1], title=title[1]) %>% 
  mutate(year=as.factor(year)) %>%
  ggplot(aes(year, n_rat)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_sqrt()

average_ratings <- movielens %>% filter(year>1992) %>% 
  group_by(title) %>%
  summarise(avgrat=mean(rating), y=year[1], avgnum = n() / (2018 - y)) %>%
  top_n(25, avgnum)

movielens %>% filter(year>1992) %>% 
  group_by(title) %>% 
  filter(n()>10) %>%
  summarise(avgr=mean(rating), y=year[1], rpy = n() / (2018 - y)) %>%
  ggplot(aes(rpy, avgr)) +
  geom_point()

movielens <- mutate(movielens, date=as_datetime(timestamp))

movielens %>% mutate(date=round_date(date, "week")) %>% 
  group_by(date) %>%
  summarise(avgr=mean(rating)) %>% 
  ggplot(aes(date, avgr)) +
  geom_point()

movielens %>% group_by(genres) %>%
  filter(n()>1000) %>%
  summarise(avg=mean(rating), sd=sd(rating)) %>%
  ggplot(aes(x=genres, y=avg ,ymin=avg-sd, ymax=avg+sd)) +
  geom_errorbar() + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
