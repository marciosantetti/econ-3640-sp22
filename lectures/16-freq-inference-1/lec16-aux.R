library(tidyverse)
library(hrbrthemes)
library(ggeasy)
library(scales)

theme_set(theme_ipsum_rc())



#------------------------------------------------------------


min <- -3.5
max <- 3.5

ggplot(data.frame(x = c(min, max)), aes(x = x)) +
  xlim(c(min, max)) + ylim(c(0, 0.45)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "#b092b1", size = 1, alpha = 0.5) +
  labs(x = "x",
       y = "f(x)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


min2 <- 500
max2 <- 1500




ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 1000, sd = 100), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 1000, sd = 100), size = 1.2, alpha = 0.8) +
  scale_x_continuous(labels = comma)

ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 1000, sd = 100), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 1000, sd = 100), size = 1.2, alpha = 0.8) +
  scale_x_continuous(labels = comma) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 1000, sd = 100), 
            fill = "#b70673", xlim = c(500, 1100), alpha = 0.4)


