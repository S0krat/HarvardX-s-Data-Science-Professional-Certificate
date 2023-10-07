library(tidyverse)
library(Lahman)
data(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game=R/G, at_bats_per_game=AB/G) %>%
  ggplot(aes(runs_per_game, at_bats_per_game)) +
  geom_point()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(WR=W/G, EPG=E/G) %>%
  ggplot(aes(WR, EPG)) +
  geom_point()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3PG=X3B/G, X2PG=X2B/G) %>%
  ggplot(aes(X3PG, X2PG)) +
  geom_point()


corr <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game=R/G, at_bats_per_game=AB/G) %>%
  summarise(corr=cor(runs_per_game, at_bats_per_game)) %>%
  .$corr
print(corr)

corr <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(WR=W/G, EPG=E/G) %>%
  summarise(corr=cor(WR, EPG)) %>%
  .$corr
print(corr)

corr <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3PG=X3B/G, X2PG=X2B/G) %>%
  summarise(corr=cor(X3PG, X2PG)) %>%
  .$corr
print(corr)
