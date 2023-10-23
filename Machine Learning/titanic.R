library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
data("titanic_train")

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42)
test_ind <- createDataPartition(titanic_clean$Survived, times=1, p=0.2, list = FALSE)
test_set <- titanic_clean %>% slice(test_ind)
train_set <- titanic_clean %>% slice(-test_ind)

mean(train_set$Survived==1)

set.seed(3)
s_hat <- sample(c(0,1), nrow(test_set), replace = TRUE)
confusionMatrix(as.factor(s_hat), test_set$Survived)$overall

train_set %>% mutate(Survived=ifelse(Survived=="0", 0, 1)) %>%
  filter(Sex=="female") %>% summarise(p=mean(Survived)) %>% .$p
train_set %>% mutate(Survived=ifelse(Survived=="0", 0, 1)) %>%
  filter(Sex=="male") %>% summarise(p=mean(Survived)) %>% .$p

s_hat <- ifelse(test_set$Sex=="female", 1, 0)
tab <- table(s_hat, test_set$Survived)
F_meas(tab)
confusionMatrix(as.factor(s_hat), test_set$Survived)

train_set %>% mutate(Survived=as.numeric(Survived) - 1) %>% group_by(Pclass) %>% 
  summarise(p = mean(Survived))

s_hat <- ifelse(test_set$Pclass==1, 1, 0)
tab <- table(s_hat, test_set$Survived)
F_meas(tab)
confusionMatrix(as.factor(s_hat), test_set$Survived)

train_set %>% mutate(Survived=as.numeric(Survived) - 1) %>% group_by(Pclass, Sex) %>% 
  summarise(p = mean(Survived))

s_hat <- ifelse((test_set$Pclass==1 | test_set$Pclass==2) & test_set$Sex=="female", 1, 0)
tab <- table(s_hat, test_set$Survived)
F_meas(tab)
confusionMatrix(as.factor(s_hat), test_set$Survived)

set.seed(1)
fit_gL <- train(Survived ~ Fare, data=train_set, method="gamLoess")
s_hat <- predict(fit_gL, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']

set.seed(1)
fit_glm <- train(Survived ~ Age, data=train_set, method="glm")
s_hat <- predict(fit_glm, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']

set.seed(1)
fit_glm <- train(Survived ~ Sex + Pclass + Fare + Age, data=train_set, method="glm")
s_hat <- predict(fit_glm, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']

set.seed(1)
fit_glm <- train(Survived ~ ., data=train_set, method="glm")
s_hat <- predict(fit_glm, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']

set.seed(6)
fit <- train(Survived ~ ., data=train_set, method="knn", 
             tuneGrid=data.frame(k=seq(3, 51, 2)))
plot(fit)
s_hat <- predict(fit, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']

set.seed(10)
fit <- train(Survived ~ ., data=train_set, method="rpart", 
             tuneGrid=data.frame(cp=seq(0, 0.05, 0.002)))
s_hat <- predict(fit, test_set)
confusionMatrix(s_hat, test_set$Survived)$overall['Accuracy']
plot(fit$finalModel, margin=0.1)
text(fit$finalModel)
