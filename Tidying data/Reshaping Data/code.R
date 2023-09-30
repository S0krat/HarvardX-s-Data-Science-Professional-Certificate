library(tidyverse)
library(dslabs)

data("co2")
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_long <- pivot_longer(co2_wide, '1':'12', values_to = 'value', names_to = 'month')

co2_long %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
