library(tidyverse)
library(hrbrthemes)


theme_set(theme_ipsum_rc())


#####=== Binomial experiment (as seen in class example):

set.seed(123)

xb <- rbinom(n = 10, size = 10, prob = 0.2)

xb <- xb %>% as_tibble()

xb %>% 
  summarize(avg = mean(value))

xb %>% 
  count(value, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(y = pct, x = value)) +
  geom_point() +
  geom_segment(aes(x = value, xend = value, y = 0, yend = pct)) +
  expand_limits(x = c(0, 10)) +
  geom_vline(xintercept = 2.4, size = 1.5, color = 'firebrick', alpha = 0.5) +
  labs(x = "x",
       y = "P(X = x)")


xb %>% 
  ggplot(aes(value)) +
  geom_histogram(color = "white", binwidth = 1) +
  expand_limits(x = c(0, 10))



########==== Binomial experiment 2:

set.seed(123)

x <- rbinom(n = 1000, size = 100, prob = 0.25)

x <- x %>% as_tibble()

x %>% 
  summarize(expected_value = mean(value))


  
x %>% 
  count(value, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(y = pct, x = value)) +
  geom_point() +
  geom_segment(aes(x = value, xend = value, y = 0, yend = pct)) +
  expand_limits(x = c(0, 60)) +
  geom_vline(xintercept = 25, size = 1.5, color = 'firebrick', alpha = 0.5) +
  labs(x = "x",
       y = "P(X = x)")

x %>% 
  ggplot(aes(value)) +
  geom_histogram(color = "white", binwidth = 1)



#######==== Poisson RV:


set.seed(123)


xp <- rpois(n = 10000, lambda = 4)

xp <- xp %>% as_tibble()

xp %>% 
  summarize(expected_value = mean(value))


xp %>% 
  count(value, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(y = pct, x = value)) +
  geom_point() +
  geom_segment(aes(x = value, xend = value, y = 0, yend = pct)) +
  expand_limits(x = c(0, 15)) +
  geom_vline(xintercept = 3.98, size = 1.5, color = 'firebrick', alpha = 0.5) +
  labs(x = "x",
       y = "P(X = x)")


xp %>% 
  ggplot(aes(value)) +
  geom_histogram(color = "white", binwidth = 1, fill = "#fa9933", alpha = 0.7)


########==== Normal distribution:


min <- -3.5
max <- 3.5

ggplot(data.frame(x = c(min, max)), aes(x = x)) +
  xlim(c(min, max)) + ylim(c(0, 0.45)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "#b092b1", size = 1, alpha = 0.5) +
  geom_vline(xintercept = 0, size = 1.5, color = "firebrick", alpha = 0.5) +
  labs(x = "x",
       y = "f(x)")



########==== Uniform distribution:

set.seed(123)


umin <- 0
umax <- 10

ggplot(data.frame(x = c(umin, umax)), aes(x = x)) +
  xlim(c(umin, umax)) + ylim(c(0, 0.15)) +
  stat_function(fun = dunif, args = list(min = umin, max = umax), geom = "area", fill = "#78c8c0", alpha = 0.35) +
  stat_function(fun = dunif, args = list(min = umin, max = umax), color = "#78c8c0", size = 1, alpha = 0.5) +
  geom_vline(xintercept = (1/2 * (umin + umax)), color = "firebrick", alpha = 0.5, size = 1) +
  labs(x = "x",
       y = "f(x)")




########

