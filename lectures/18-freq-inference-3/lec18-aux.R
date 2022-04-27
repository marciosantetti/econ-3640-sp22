library(tidyverse)
library(hrbrthemes)
library(ggeasy)
library(scales)

theme_set(theme_ipsum_rc())


min2 <- -3.5
max2 <- 3.5

ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.2, alpha = 0.8) +
  stat_function(fun = dt, args = list(df = 1), geom = "area", fill = "#d95d2c", alpha = 0.2) +
  stat_function(fun = dt, args = list(df = 1), size = 0.8, alpha = 0.8) +
  theme(axis.text.x=element_blank()) +
  labs(x = "x", y = "",
       title = "Standard Normal and t distributions") +
  easy_x_axis_title_size(13)


ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 1), size = 0.4, alpha = 0.8) +
  stat_function(fun = dt, args = list(df = 2), size = 0.5, alpha = 0.8, color = "#d95d2c") +
  stat_function(fun = dt, args = list(df = 5), size = 0.6, alpha = 0.8, color = "#33aa66") +
  stat_function(fun = dt, args = list(df = 10), size = 0.7, alpha = 0.8, color = "#9d43a5") +
  stat_function(fun = dt, args = list(df = 30), size = 0.8, alpha = 0.8, color = "#4c2b50") +
  theme(axis.text.x=element_blank()) +
  labs(x = "x", y = "",
       title = "t distributions with different d.o.f.") +
  easy_x_axis_title_size(13)



min2 <- -3.5
max2 <- 3.5

breakk = 1.64


labelss = as.character(breakk)

ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.2, alpha = 0.8) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
            fill = "#b70673", xlim = c(1.64, 3.5), alpha = 0.4) +
  labs(x = "z", y = "") +
  scale_x_continuous(breaks = breakk, labels = labelss) +
  easy_x_axis_labels_size(13) +
  easy_x_axis_title_size(13)



c_values <- c(-1.96, 1.96)

labss <- as.character(c_values)


ggplot(data.frame(x = c(min2, max2)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.2, alpha = 0.8) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
            fill = "#b70673", xlim = c(1.96, 3.5), alpha = 0.4) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
            fill = "#b70673", xlim = c(-1.96, -3.5), alpha = 0.4) +
  labs(x = "z", y = "") +
  scale_x_continuous(breaks = c_values, labels = labss) +
  easy_x_axis_labels_size(13) +
  easy_x_axis_title_size(13)

