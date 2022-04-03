library(tidyverse)
library(bayesrules)
library(hrbrthemes)
library(ggeasy)
library(patchwork)


theme_set(theme_ipsum_rc())


#-------------------------------------------



set.seed(123)

dat <- tibble(
  x = rpois(1000, lambda = 1),
  x2 = rpois(1000, lambda = 5),
  x3 = rpois(1000, lambda = 10),
  x4 = rpois(1000, lambda = 15)
)

dat %>% 
  count(x, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = x, y = pct)) +
  geom_point() +
  geom_segment(aes(x = x, xend = x, y = 0, yend = pct)) +
  labs(x = "y",
       y = "f(y)",
       title = expression(lambda~"= 1"))

dat %>% 
  count(x2, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = x2, y = pct)) +
  geom_point() +
  geom_segment(aes(x = x2, xend = x2, y = 0, yend = pct)) +
  labs(x = "y",
       y = "f(y)",
       title = expression(lambda~"= 5"))


dat %>% 
  count(x3, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = x3, y = pct)) +
  geom_point() +
  geom_segment(aes(x = x3, xend = x3, y = 0, yend = pct)) +
  labs(x = "y",
       y = "f(y)",
       title = expression(lambda~"= 10"))


dat %>% 
  count(x4, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = x4, y = pct)) +
  geom_point() +
  geom_segment(aes(x = x4, xend = x4, y = 0, yend = pct)) +
  labs(x = "y",
       y = "f(y)",
       title = expression(lambda~"= 15"))



#---------------


set.seed(123)

datt <- tibble(
  x = rgamma(1000, shape = 1, rate = 1),
  x2 = rgamma(1000, shape = 5, rate = 2),
  x3 = rgamma(1000, shape = 2, rate = 5),
  x4 = rgamma(1000, shape = 3, rate = 3)
)


p5 <- datt %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 1, rate = 1), size = 0.7) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "s = 1, r = 1") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

p6 <- datt %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 5, rate = 2), size = 0.7) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "s = 5, r = 2") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

p7 <- datt %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 2, rate = 5), size = 0.7) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "s = 2, r = 5") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

p8 <- datt %>% 
  ggplot(aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 3, rate = 3), size = 0.7) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "s = 3, r = 3") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 6, rate = 1), size = 0.7) +
  scale_x_continuous(breaks = seq(0,15,2)) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "Gamma(6, 1)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 12, rate = 2), size = 0.7) +
  scale_x_continuous(breaks = seq(0,15,2)) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "Gamma(12, 2)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

6 + 10 + 3 + 5 + 7 + 6 + 6 + 10 + 3 + 5


plot_poisson_likelihood(y = c(6, 10, 3, 5, 7, 6, 6, 10, 3, 5), lambda_upper_bound = 15)


plot_gamma_poisson(shape = 12, rate = 2, sum_y = 61, n = 10)



pp <- ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 6, rate = 1), size = 0.7) +
  scale_x_continuous(breaks = seq(0,15,2)) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "Prior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

pp1 <- plot_poisson_likelihood(y = c(6, 10, 3, 5, 7, 6, 6, 10, 3, 5), lambda_upper_bound = 15) +
  labs(title = "Likelihood")

pp2 <- ggplot(data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = dgamma, args = list(shape = 6 + 61, rate = 1 + 10), size = 0.7) +
  scale_x_continuous(breaks = seq(0,15,2)) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "Posterior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

(pp | pp1) / pp2
