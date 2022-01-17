library(tidyverse)
library(ggeasy)
library(wooldridge)
library(tidytuesdayR)
library(hrbrthemes)
library(showtext)
library(scales)



titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')



data <- tibble(
  
  set1 <- rnorm(10000, mean=5, sd=3),
  set2 <- rnorm(10000, mean=5, sd=5) 
  
)


data %>% ggplot() +
  geom_density(aes(x = set1, y = ..density..), fill = "#67dfc4", alpha = 0.5) +
  geom_density(aes(x = set2, y = ..density..), fill = "#b6134a", alpha = 0.6) +
  labs(x = 'Values', y = 'Density', title = "Same mean, different variances") +
  theme_ipsum_rc()



####

theme_set(theme_ipsum_rc())

titles %>% 
  filter(type == "Movie",
         ! rating %in% NA) %>% 
  separate(duration, c("duration_mins", "B")) %>% 
  mutate(duration_mins = as.integer(duration_mins),
         rating = fct_reorder(rating, duration_mins)) %>% 
  ggplot(aes(x = duration_mins, y = rating)) +
  geom_boxplot() +
  labs(x = "Duration (mins)",
       y = "Movie Ratings",
       title = "Netflix movie durations (in minutes)") +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12) +
  scale_x_continuous(breaks = seq(0,350,50))

  
