library(tidyverse)
library(hrbrthemes)
library(bayesrules)
library(ggeasy)

theme_set(theme_ipsum_rc())



####################################


data <- 
  tibble(
    p_grid = seq( from=0 , to=1 , length.out=1000 ),
    prior = rep( 1 , 1000 ),
    likelihood = dbinom( 1 , size=1 , prob=p_grid ),
    likelihood2 = dbinom( 1 , size=2 , prob=p_grid ),
    likelihood3 = dbinom( 2 , size=3 , prob=p_grid ),
    likelihood4 = dbinom( 3 , size=4 , prob=p_grid ),
    likelihood5 = dbinom( 3 , size=5 , prob=p_grid ),
    likelihood6 = dbinom( 3 , size=6 , prob=p_grid ),
    likelihood7 = dbinom( 3 , size=7 , prob=p_grid ),
    likelihood8 = dbinom( 3 , size=8 , prob=p_grid ),
    unstd_posterior = likelihood * prior,
    unstd_posterior2 = likelihood2 * prior,
    unstd_posterior3 = likelihood3 * prior,
    unstd_posterior4 = likelihood4 * prior,
    unstd_posterior5 = likelihood5 * prior,
    unstd_posterior6 = likelihood6 * prior,
    unstd_posterior7 = likelihood7 * prior,
    unstd_posterior8 = likelihood8 * prior,
    norm_posterior = unstd_posterior/sum(unstd_posterior)
  )


data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = prior), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior2)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior3)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood2), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior4)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood3), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior5)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood4), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior6)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood5), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior7)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood6), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(x = p_grid, y = unstd_posterior8)) +
  geom_line(size = 1) +
  geom_line(aes(x = p_grid, y = likelihood7), linetype = 2) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

