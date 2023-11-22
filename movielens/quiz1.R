library(readr)
library(tidyverse)

edx <- read_rds("edx.Rds")
fht <- read_rds("fht.Rds")

nrow(edx)
ncol(edx)

sum(edx$rating==0)
sum(edx$rating==3)

length(unique(edx$movieId))
length(unique(edx$userId))

sum(grepl("Drama", edx$genres, fixed = TRUE))
sum(grepl("Comedy", edx$genres, fixed = TRUE))
sum(grepl("Thriller", edx$genres, fixed = TRUE))
sum(grepl("Romance", edx$genres, fixed = TRUE))

edx %>% group_by(movieId) %>% 
  summarise(n=n(), title=title[1]) %>% 
  top_n(10, n) %>% 
  arrange(desc(n))

edx %>% group_by(rating) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>%
  arrange(desc(n))
