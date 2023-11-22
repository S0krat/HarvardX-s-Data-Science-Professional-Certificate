library(readr)
library(tidyverse)

# edx <- read_rds("edx.Rds")
# fht <- read_rds("fht.Rds")
edx <- read.csv("C:\\Users\\limon\\OneDrive\\Документы\\HarvardX-s-Data-Science-Professional-Certificate\\ml-latest\\ratings.csv")
movie_map <- read.csv("C:\\Users\\limon\\OneDrive\\Документы\\HarvardX-s-Data-Science-Professional-Certificate\\ml-latest\\movies.csv")
movie_map <- select(movie_map, movieId, title)

edx <- edx %>% group_by(movieId) %>%
  filter(n() > 50) %>%
  ungroup()

edx <- edx %>% group_by(userId) %>%
  filter(n() > 20) %>%
  ungroup()

# write.csv(edx, "C:\\Users\\limon\\OneDrive\\Документы\\HarvardX-s-Data-Science-Professional-Certificate\\ml-latest\\ratings.csv")
# rated <- movie_map %>% mutate(rating=NA)
# write.csv(rated, "C:\\Lev\\rated.csv")

edx <- select(edx, movieId, userId, rating)

mu <- mean(edx$rating)

# Regularization: alpha_f = 5
fit_movies <- edx %>% group_by(movieId) %>%
  mutate(rating=rating-mu) %>%
  summarise(b_i=sum(rating) / (n() + 5), movieId=movieId[1], mu=mu)

# write_rds(fit_movies, "~/HarvardX-s-Data-Science-Professional-Certificate/fit_movies.Rds")
# fit_movies <- read_rds("fit_movies.Rds")

# Regularization: alpha_u = 5
fit_users <- edx %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(rating=rating-mu-b_i) %>%
  group_by(userId) %>%
  summarise(b_u=sum(rating) / (n() + 5), userId=userId[1])

# write_rds(fit_users, "~/HarvardX-s-Data-Science-Professional-Certificate/fit_users.Rds")
fit_users <- read_rds("fit_users.Rds")
mu = 3.54789373

biased_edx <- edx %>%
  left_join(fit_users, by="userId") %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(user_bias=rating-mu-b_u-b_i) %>%
  select(movieId, userId, user_bias) %>%
  arrange(movieId)

write_rds(biased_edx, "~/HarvardX-s-Data-Science-Professional-Certificate/biased_edx.Rds")

rm(edx)
