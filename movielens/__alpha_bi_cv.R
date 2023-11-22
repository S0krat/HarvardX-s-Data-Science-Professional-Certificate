library(tidyverse)
library(dslabs)
library(caret)
library(readr)

edx <- read_rds("edx.Rds")

set.seed(124)

alphas <- seq(1.4, 2, 0.02)

edx <- edx %>% select(movieId, userId, rating)

n <- nrow(edx)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

val_index <- createDataPartition(edx$movieId, times=1, p=0.1, list=FALSE)

train_set <- edx[-val_index,] %>% pivot_wider(names_from = movieId, values_from = rating)
validate_set <- edx[val_index,]
train_set <- as.matrix(train_set[,-1])

rm(edx, val_index)

fit_movies <- data.frame(movieId = as.integer(colnames(train_set)))
mu <- mean(train_set, na.rm = TRUE)
ns <- colSums(!is.na(train_set))
sums <- colSums(train_set - mu, na.rm = TRUE)

rmses <- sapply(alphas, function(a){
  b_i <- sums / (ns + a)
  fit_movies$b_i <- b_i
  inner_join(validate_set, fit_movies, by = "movieId") %>% mutate(pred = mu + b_i) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    .$rmse
})

print(alphas[which.min(rmses)])
      