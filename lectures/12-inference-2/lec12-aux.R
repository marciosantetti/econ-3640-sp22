library(tidyverse)
library(hrbrthemes)
library(ggeasy)
library(bayesrules)


theme_set(theme_ipsum_rc())

######################################


## Playing around with the Beta distribution:


ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 5)) +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 5), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(1, 5)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 5)) +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 1), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(5, 1)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 2)) +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 2), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(3, 2)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 2)) +
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 2), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(2, 2)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 5.5)) +
  stat_function(fun = dbeta, args = list(shape1 = 20, shape2 = 20), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(20, 20)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 8)) +
  stat_function(fun = dbeta, args = list(shape1 = 50, shape2 = 30), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(50, 30)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 8)) +
  stat_function(fun = dbeta, args = list(shape1 = 50, shape2 = 30), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(50, 30)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



#####


# Tuning:

ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 9)) +
  stat_function(fun = dbeta, args = list(shape1 = 20, shape2 = 60), color = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)",
       title = "X ~ Beta(20, 60)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


######


dat <- 
  tibble(
    p_grid = seq( from=0 , to=1 , length.out=1000 ),
    prior = dbeta(p_grid, shape1 = 20, shape2 = 60),
    likelihood = dbinom(x = 30, size = 100, prob = p_grid),
    posterior = prior * likelihood,
    std_posterior = posterior / sum(posterior)
  )

dat %>% 
  ggplot(aes(x = p_grid, y = prior)) +
  geom_line(size = 0.8) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Prior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


dat %>% 
  ggplot(aes(x = p_grid, y = likelihood)) +
  geom_line(size = 0.8) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Likelihood") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


dat %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_line(size = 0.8) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Posterior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)
