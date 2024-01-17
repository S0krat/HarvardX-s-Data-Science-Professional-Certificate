library(readr)
library(caret)
library(tidyverse)

val_set <- read_rds("movielens/validate_set.Rds")
val_set <- select(val_set, userId, movieId, rating)

set.seed(10)

test_index <- createDataPartition(as.factor(val_set$userId), times = 1, p = 0.1, list = FALSE)
test_val <- val_set[test_index,]
train_val <- val_set[-test_index,]
rm(val_set, test_index)

fit_movies <- read_rds("movielens/fit_movies.Rds")

fit_users <- train_val %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(rating=rating-b_i) %>%
  group_by(userId) %>%
  summarise(b_u=mean(rating))

RMSE <- function(a, b) {
  sqrt(sum((a-b)**2)/length(a))
}

train_val_biased <- train_val %>%
  left_join(fit_movies, by="movieId") %>%
  left_join(fit_users, by="userId") %>%
  mutate(user_bias=rating-b_u-b_i) %>%
  select(userId, movieId, user_bias)

biased_train_set <- read_rds("movielens/biased_train_set.Rds")

test_val <- mutate(test_val, bias=0)

i <- 1

val_users <- unique(train_val_biased$userId)
isss <- length(val_users)

id = 10
for (id in val_users) {

  warningsprint(i/isss)

  i <<- i + 1
  
  tt <- filter(train_val_biased, userId==id) %>%
    select(movieId, user_bias)
  train_movies <- filter(train_val, userId==id) %>% .$movieId
  test_movies <- filter(test_val, userId==id) %>% .$movieId
  n_movies <- length(train_movies)
  
  genius <- biased_train_set %>%
    filter(movieId %in% train_movies) %>%
    group_by(userId) %>%
    filter(n() > 7) %>%
    ungroup() %>%
    pivot_wider(names_from = movieId, values_from = user_bias)
  
  corrs <- sapply(1:nrow(genius), function(i) {
    prod <- tt$user_bias * as.numeric(genius[i, 1:n_movies+1])
    sum(prod[!is.na(prod)])
  })
  corrs = data.frame(userId=genius$userId,corr=corrs)
  corrs$corr <- corrs$corr / sum(abs(corrs$corr))
  
  genius_other <- biased_train_set %>%
    filter(userId %in% corrs$userId & movieId %in% test_movies) %>%
    left_join(corrs, by="userId") %>%
    group_by(movieId) %>%
    summarise(rec_bias=sum(user_bias * corr))
  
  ids <- which(test_val$userId==id)
  test_val$bias[ids] <- genius_other$rec_bias
}

test_val <- filter(test_val, bias != 0)
test_val <- test_val %>% 
  left_join(fit_movies, by="movieId") %>% 
  left_join(fit_users, by="userId") %>%
  mutate(predict=b_u+b_i) %>%
  select(rating, bias, predict)

# write_rds(test_val, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/test_val2.Rds")

alpha <- seq(0, 50, 1)
rmses <- sapply(alpha, function(a) {
  test_val %>% filter(!is.na(bias)) %>%
    mutate(final=predict+a*bias) %>%
    summarise(rmse=RMSE(rating, final)) %>%
    .$rmse
})

alpha[which.min(rmses)] # 0.81
min(rmses) # 0.7933
plot(alpha, rmses)
