models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

predictions <- sapply(fits, function(fit) {
  predict(fit, mnist_27$test)
})
predictions <- data.frame(predictions)

acc <- sapply(predictions, function(pred) {
  mean(pred==mnist_27$test$y)
})

mean(acc)

t_pred <- data.frame(t(predictions))

ensemble_pred <- sapply(t_pred, function(chrs) {
  ifelse(mean(chrs=="2") > 0.5, "2", "7")
})

mean(ensemble_pred==mnist_27$test$y)

acc_est <- sapply(fits, function(fit){
  max(fit$results$Accuracy)
})

acc_est