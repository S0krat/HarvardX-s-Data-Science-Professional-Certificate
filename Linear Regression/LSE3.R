library(tidyverse)
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>% 
  summarise(mean_singles=mean(singles), mean_bb=mean(bb))

bat_9901 %>% filter(mean_singles > 0.2) %>% nrow()
bat_9901 %>% filter(mean_bb > 0.2) %>% nrow()

bat <- bat_02 %>% inner_join(bat_9901, by='playerID')

cor(bat$singles, bat$mean_singles)
cor(bat$bb, bat$mean_bb)

library(gridExtra)

plot1 <- bat %>% ggplot(aes(mean_singles, singles)) + geom_point()
plot2 <- bat %>% ggplot(aes(mean_bb, bb)) + geom_point()
grid.arrange(plot1, plot2, ncol=2)

bat %>% lm(singles~mean_singles, data=.) %>% .$coef
bat %>% lm(bb~mean_bb, data=.) %>% .$coef

