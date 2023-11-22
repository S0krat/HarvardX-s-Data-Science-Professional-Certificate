library(readr)
library(tidyverse)

biased_edx <- read_rds("biased_edx.Rds")
fit_movies <- read_rds("fit_movies.Rds")
movie_map <- read_rds("movie_map.Rds")

rated <- read.csv(file="C:\\Lev\\rated.csv")
rated <- rated %>% filter(!is.na(rating)) %>% mutate(rating = rating / 2)
rated <- rated %>% left_join(fit_movies, by="movieId") %>% 
  mutate(bias=rating-mu-b_i) %>% 
  select(title, movieId, rating, bias)

my_mu <- mean(rated$bias)
rated <- rated %>% mutate(bias = bias - my_mu)
rated <- arrange(rated, movieId)

genius <- biased_edx %>%
  filter(movieId %in% rated$movieId) %>% 
  group_by(userId) %>%
  filter(n() > 10) %>% 
  ungroup() %>%
  pivot_wider(names_from = movieId, values_from = user_bias)

corrs <- sapply(1:nrow(genius), function(i) {
  #cor(rated$bias, as.numeric(genius[i, 2:29]), use = "complete.obs")
  prod = rated$bias * as.numeric(genius[i, 2:29])
  sum(prod[!is.na(prod)])
})
corrs = data.frame(userId=genius$userId,corr=corrs)
corrs = corrs[corrs$corr > 0,]
# corrs$corr = exp(corrs$corr * 2) - 1
corrs$corr <- corrs$corr / sum(corrs$corr)

genius_other <- biased_edx %>%
  filter(userId %in% corrs$userId) %>%
  filter(!movieId %in% rated$movieId) %>% 
  left_join(corrs, by="userId") %>% 
  group_by(movieId) %>%
  summarise(rec_bias=sum(user_bias * corr)) %>%
  arrange(desc(rec_bias)) 

genius_other %>% 
  left_join(movie_map, by="movieId") %>%
  top_n(10, rec_bias)

genius_other %>% 
  left_join(movie_map, by="movieId") %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(p_rating=mu+b_i+3*rec_bias) %>%
  select(movieId ,title, rec_bias, p_rating) %>%
  arrange(desc(p_rating)) %>%
  top_n(10, p_rating)
