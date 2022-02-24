library(tidyverse)
library(hrbrthemes)
library(ggeasy)
library(plotly)
library(skimr)


theme_set(theme_ipsum_rc())

#------------------------------------


data <- read_csv("cdc_data.csv")

h <- data %>% 
  ggplot(aes(poverty_rate)) +
  geom_histogram(color = "white", fill = "#800080", binwidth = 1) +
  labs(x = "Poverty rate in the US (1999 - 2017)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

data %>% 
  ggplot(aes(poverty_rate)) +
  geom_histogram( aes(y = ..density..),color = "white", fill = "#800080", binwidth = 0.11) +
  geom_density(alpha = .3, size = 1) +
  labs(x = "Poverty rate in the US (1999 - 2017)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

h %>% ggplotly()


hh <- data %>% 
  ggplot(aes(poverty_rate)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#800080", binwidth = 1) +
  labs(x = "Poverty rate in the US (1999 - 2017)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


hh %>% ggplotly()


data %>% 
  ggplot(aes(poverty_rate)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#800080", binwidth = 1, alpha = 0.5) +
  geom_density(alpha = .3, size = 1) +
  labs(x = "Poverty rate in the US (1999 - 2017)") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


########

set.seed(123)

x <- runif(10000000, min = 5, max = 10)

x <- x %>% as_tibble()


x %>% 
  count(value, sort=TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'white', binwidth = 1.5)


curve(dnorm(x, mean = 0, sd = 1), xlim = c(-5, 5), col = "#1478a7", lwd = 2,
      ylab = "Density",
      main = "Different means, same SD")
curve(dnorm(x, mean = 1, sd = 1), add = TRUE, col = "#701718", lwd = 2)
curve(dnorm(x, mean = -1, sd = 1), add = TRUE, col = '#809b94', lwd = 2)
