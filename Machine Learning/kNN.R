library(dslabs)
library(caret)
data("heights")

set.seed(1)
test_ind <- createDataPartition(heights$sex, times=1, p=0.5, list = FALSE)

heights_train <- heights[-test_ind,]
heights_test <- heights[test_ind,]

k <- seq(1, 101, 3)

fit <- knn3(sex ~ height, data=heights_train, k=5)
y_hat <- predict(fit, heights_test, type="class")
tab <- table(y_hat, as.numeric(heights_test$sex))

F1_scores <- sapply(k, function(x) {
  fit <- knn3(sex ~ height, data=heights_train, k=x)
  y_hat <- predict(fit, heights_test, type="class")
  tab <- table(y_hat, heights_test$sex)
  f <- F_meas(tab)
  c(k=x, f1=f)
})

max(F1_scores[2,])
