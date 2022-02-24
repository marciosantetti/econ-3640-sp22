library(tidyverse)
library(ggeasy)

coin_flip <- tibble(
  outcomes = c("HH", "HT", "TH", "TT"),
  num_value = c(2, 1, 1, 0)
)


coin_flip %>% 
  count(num_value, sort=TRUE) %>% 
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(x = num_value, y = pct)) +
  geom_point(size=2) +
  geom_segment(aes(x = num_value, xend = num_value, y = 0, yend = pct)) +
  expand_limits(x = c(-1,3),
                y = c(0,1)) +
  labs(y = "PMF",
       x = "x",
       title = "Probability mass function for 2 coin flips") +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12)
