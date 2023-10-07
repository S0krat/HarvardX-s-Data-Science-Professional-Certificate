library(tidyverse)
library(Lahman)
data(Batting)
data(People)

top <- Batting %>% 
  filter(yearID==2016) %>%
  arrange(desc(HR)) %>% 
  slice(1:10)

top <- top %>% as_tibble()
People <- People %>% as_tibble()

top_names <- left_join(top, People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, HR)

data(Salaries)

top_salaries <- Salaries %>% filter(yearID==2016) %>% 
  right_join(top_names, by="playerID") %>% 
  select(playerID, nameFirst, nameLast, teamID, HR, salary)

data("AwardsPlayers")

top_awards <- AwardsPlayers %>% filter(yearID==2016) %>% 
  inner_join(top_salaries, by="playerID") %>% 
  select(playerID, nameFirst, nameLast, teamID, HR, salary, awardID)

awarded_in_top10 <- length(unique(top_awards$playerID))

length(unique(filter(AwardsPlayers, yearID==2016)$playerID))
