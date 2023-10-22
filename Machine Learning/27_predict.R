library(dslabs)
data("mnist_27")
library(broom)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

fit <- loess(as.numeric(y)~x_2, data=mnist_27$train, degree = 1)

p <- predict(fit, mnist_27$test)
p <- ifelse(p<1.5, 2, 7)
mean(p==mnist_27$test$y)
