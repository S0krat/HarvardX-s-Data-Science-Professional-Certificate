library(tidyverse)

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools %>% top_n(10, score) %>% arrange(desc(score))

median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

schools %>% top_n(10, -score) %>% arrange(score)
schools %>% top_n(10, -score) %>% .$size %>% median()

schools %>% mutate(size=as.factor(round(size, -2))) %>% 
  ggplot(aes(size, score)) +
  geom_boxplot()

overall <- mean(sapply(scores, mean))

alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools <- schools %>% mutate(reg_score = score_reg)


schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))

alpha = 10:250
reg_scores <- sapply(alpha, function(a) {
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+a))
  sqrt(mean((schools$quality - score_reg)^2))
})
alpha <- alpha[which.min(reg_scores)]

score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools <- schools %>% mutate(reg_score = score_reg)
schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))



alpha = 10:250 # MISTAKE
reg_scores <- sapply(alpha, function(a) {
  score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+a))
  sqrt(mean((schools$quality - score_reg)^2))
})
alpha <- alpha[which.min(reg_scores)]

score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+alpha))
schools <- schools %>% mutate(reg_score = score_reg)
schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))
