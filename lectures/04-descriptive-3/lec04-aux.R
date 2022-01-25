library(tidyverse)
library(wooldridge)
library(hrbrthemes)
library(scales)
library(ggeasy)

###

theme_set(theme_ipsum_rc())

data("hprice3")


df <- hprice3 %>% as_tibble()


df %>% 
  ggplot(aes(x = rooms, y = price)) +
  geom_point(color = "#3c6e91", alpha = 0.6) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Number of rooms",
       y = "Selling price ($)",
       title = "House prices vs. number of rooms") +
  easy_y_axis_title_size(12) +
  easy_x_axis_title_size(12)


df %>% 
  filter(! area > 5000) %>% 
  ggplot(aes(x = area, y = price)) +
  geom_point(color = "#dd2cc6", alpha = 0.6) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = comma) +
  labs(x = "Square footage",
       y = "Selling price ($)",
       title = "House prices vs. square footage") +
  easy_y_axis_title_size(12) +
  easy_x_axis_title_size(12)


df %>% 
  ggplot(aes(y = land, x = nbh)) +
  geom_point(color = "#af8914", alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(x = "Neighborhood rating",
       y = "Lot square footage",
       title = "Lot square footage vs. neighborhood evaluations") +
  easy_y_axis_title_size(12) +
  easy_x_axis_title_size(12)


df %>% 
  count(rooms, sort = TRUE) # for mode.

#####


data("smoke")


smoke <- as_tibble(smoke)

smoke_filtered <- smoke %>% 
  filter(cigs > 0)

smoke_filtered %>% 
  summarize(covariance_cigpric_cigs = cov(cigpric, cigs),
            correlation_cigpric_cigs = cor(cigpric, cigs))


smoke_filtered %>% 
  ggplot(aes(x = cigpric, y = cigs)) +
  geom_point()


smoke_filtered %>% 
  ggplot(aes(x = cigpric, y = cigs)) +
  geom_point(color = "#6e549a", alpha = 0.6) +
  labs(x = "Cigarette pack price (cents per pack)",
       y = "Number of cigarettes smoked per day",
       title = "Cigarette consumption vs. state price")



smoke_filtered %>% 
  summarize(covariance_educ_cigs = cov(educ, cigs),
            correlation_educ_cigs = cor(educ, cigs))

smoke_filtered %>% 
  ggplot(aes(x = educ, y = cigs)) +
  geom_point()


smoke_filtered %>% 
  summarize(covariance_age_cigs = cov(age, cigs),
            correlation_age_cigs = cor(age, cigs))

smoke_filtered %>% 
  ggplot(aes(x = age, y = cigs)) +
  geom_point()

