library(tidyverse)
library(wooldridge)

data("affairs")

affairs %>% 
  as_tibble() %>% 
  ggplot() +
  geom_histogram(aes(x = yrsmarr), binwidth = 5, color = "white") +
  scale_x_continuous(breaks = seq(0, 70, 5))
