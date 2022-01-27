library(tidyverse)
library(hrbrthemes)
library(skimr)
library(wooldridge)
library(ggrepel)


theme_set(theme_ipsum())

board_games <- read_csv("board-games.csv")


board_games %>% 
  count(yearpublished)   ## ??


board_games %>% 
  filter(yearpublished > 1960) %>% 
  ggplot(aes(x = yearpublished)) +
  geom_histogram(binwidth = 5, color = "white")

board_filtered <- board_games %>% 
  filter(yearpublished > 1960) 

board_filtered %>% skim()

board_filtered %>% 
  summarize(avg_minplayers = mean(minplayers),
            avg_maxplayers = mean(maxplayers),
            avg_minage = mean(minage))
  

#####

data("prison")

prison <- prison %>% as_tibble()

prison %>% skim()

prison %>% 
  count(state)


prison %>% 
  filter(year == "90") %>% 
  ggplot(aes(x = unem, y = criv)) +
  geom_point()

prison %>% 
  filter(year == "90") %>% 
  ggplot(aes(x = incpc, y = criv)) +
  geom_point()

prison %>% 
  filter(year == "90") %>% 
  ggplot(aes(x = criv, y = pris)) +
  geom_point()

prison %>% 
  filter(year == "90") %>% 
  ggplot(aes(x = metro, y = criv)) +
  geom_point()



prison %>% 
  mutate(year = as_factor(year)) %>% 
  filter(year %in% c("83", "89", "90")) %>% 
  ggplot(aes(x = criv, y = pris)) +
  geom_point() +
  facet_wrap(~ year, ncol = 1)


prison %>% 
  summarize(cor(unem, criv),
            cov(unem, criv),
            cor(unem, criv)^2 * 100,
            cor(criv, pris))
