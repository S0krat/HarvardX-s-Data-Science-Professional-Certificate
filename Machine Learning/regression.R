library(tidyverse)
library(caret)

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

Q2_function <- function(nnn) {
  dat <- MASS::mvrnorm(n = nnn, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(100, {
    train_inds <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    dat_test <- dat[train_inds,]
    dat_train <- dat[-train_inds,]
    
    model <- lm(y ~ x, data=dat_train)
    predict_test <- predict(model, dat_test)
    sqrt(mean((predict_test - dat_test$y) ** 2))
  })
  
  c(avg = mean(RMSE), sd = sd(RMSE))
}

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, Q2_function)



set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

model1 <- lm(y ~ x_1, data = train_set)
predict1 <- predict(model1, test_set)
RMSE1 <- sqrt(mean((predict1 - test_set$y)**2))

model2 <- lm(y ~ x_2, data = train_set)
predict2 <- predict(model2, test_set)
RMSE2 <- sqrt(mean((predict2 - test_set$y)**2))

model3 <- lm(y ~ x_1 + x_2, data = train_set)
predict3 <- predict(model3, test_set)
RMSE3 <- sqrt(mean((predict3 - test_set$y)**2))

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

model1 <- lm(y ~ x_1, data = train_set)
predict1 <- predict(model1, test_set)
RMSE1 <- sqrt(mean((predict1 - test_set$y)**2))

model2 <- lm(y ~ x_2, data = train_set)
predict2 <- predict(model2, test_set)
RMSE2 <- sqrt(mean((predict2 - test_set$y)**2))

model3 <- lm(y ~ x_1 + x_2, data = train_set)
predict3 <- predict(model3, test_set)
RMSE3 <- sqrt(mean((predict3 - test_set$y)**2))
