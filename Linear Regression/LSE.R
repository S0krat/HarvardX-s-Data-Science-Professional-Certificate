library(tidyverse)
library(HistData)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>% filter(gender=="male") %>% 
  select(father, childHeight) %>%
  rename(son=childHeight)

rss <- function(beta1, beta0) {
  s <- galton_heights$son - beta1 * galton_heights$father - beta0
  sum(s**2)
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
