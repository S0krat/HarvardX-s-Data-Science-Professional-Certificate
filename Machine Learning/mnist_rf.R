library(dslabs)
mnist <- read_mnist()

set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

library(randomForest)
index <- sample(nrow(x), 1000)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50))
train_rf <-  train(x[index, col_index], y[index],
                   method = "rf",
                   nTree = 100,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 3000)

fit_rf <- randomForest(x[, col_index], y,
                       minNode = train_rf$bestTune$mtry)
y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]