library(rpart)
library(tidyverse)
library(dslabs)
data("tissue_gene_expression")

tge = data.frame(tissue_gene_expression)

set.seed(1991)
fit <- caret::train(y ~ ., 
                    method="rpart", 
                    tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)),
                    data=tge)
plot(fit)

set.seed(1991)
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

plot(fit_rpart$finalModel, margin=0.1)
text(fit_rpart$finalModel, cex = 0.75)
