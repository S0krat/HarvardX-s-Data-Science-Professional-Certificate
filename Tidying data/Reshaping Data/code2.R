library(tidyverse)
library(dslabs)

data(admissions)
dat <- admissions %>% select(-applicants)

dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")

tmp2 <- unite(tmp, column_name, c(key, gender))
