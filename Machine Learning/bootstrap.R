library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

sum(sapply(indexes, function(l) {
  v <- unlist(l)
  sum(v==3)
}))

set.seed(1)
B <- 10000
q75s <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q75s)
sd(q75s)

set.seed(1)
y <- rnorm(100, 0, 1)
resample_ind <- createResample(y, B)
q75s_bt <- sapply(resample_ind, function(l) {
  quantile(y[unlist(l)], 0.75) 
})
mean(q75s_bt)
sd(q75s_bt)
