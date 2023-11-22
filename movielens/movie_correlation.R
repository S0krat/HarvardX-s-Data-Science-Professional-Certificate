library(readr)
library(tidyverse)

biased_edx <- read_rds("biased_edx.Rds")
fit_movies <- read_rds("fit_movies.Rds")
movie_map <- read_rds("movie_map.Rds")

users306 <- biased_edx %>%
  filter(movieId==306) %>% 
  rename("movie_bias"="user_bias") %>%
  select(userId, movie_bias)

movie_corr <- biased_edx %>%
  group_by(userId) %>%
  inner_join(users306, by="userId") %>%
  group_by(movieId) %>%
  summarise(bias=sum(user_bias * movie_bias)) %>%
  left_join(movie_map, by="movieId")

movie_corr %>%
  arrange(desc(bias)) %>%
  top_n(10, bias)
