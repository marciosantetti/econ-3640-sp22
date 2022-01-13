library(tidyverse)
library(ggeasy)
library(wooldridge)
library(tidytuesdayR)
library(hrbrthemes)
library(showtext)
library(scales)



titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')


font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")


titles %>% 
  filter(type == "Movie") %>% 
  count(rating, sort=TRUE) %>%
  filter(! rating %in% c(NA, "TV-Y7-FV", "UR", "NC-17")) %>% 
  ggplot() + geom_col(aes(y = fct_reorder(rating, n), x = n), fill = "steelblue4") +
  theme_ipsum_rc() +
  labs(x = '# Movies', y = "Ratings",
       title = 'Netflix movies by category') +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


titles %>% 
  count(type, sort = TRUE) %>% 
  mutate(share = round(n/sum(n)*100, 2)) %>% 
  ggplot(aes(x="", y = n, fill = type)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  easy_remove_x_axis() +
  easy_remove_y_axis() +
  labs(title = "Netflix catalog by type") +
  theme(text = element_text(family = "Roboto Condensed")) +
  geom_text(aes(label = paste0(share, "%")), position = position_stack(vjust=0.5),
            color=c("white", "black")) +
  viridis::scale_fill_viridis(discrete = TRUE, option = "D") +
  easy_add_legend_title("Movie or TV Show?") +
  easy_plot_legend_size(11) +
  easy_plot_title_size(17)

  
titles %>% 
  filter(type == "Movie") %>% 
  separate(duration, c("duration_mins", "B")) %>% 
  mutate(duration_mins = as.integer(duration_mins)) %>% 
  ggplot() + 
  geom_histogram(aes(duration_mins), bins = 12, color = "white", fill = "lightcoral") +
  theme_ipsum_rc() +
  labs(title = "Netflix movie lengths in minutes", y = "Count", x = "Minutes") +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12)


data("sleep75")

sleep75 %>% as_tibble() %>% 
  ggplot(aes(y = sleep, x = worknrm)) +
  geom_point(size = 2, color = "brown1", alpha = 0.7) +
  labs(x = "Minutes worked per week", y = "Minutes slept per week",
       title = "Sleeping vs. working time") +
  theme_ipsum_rc() +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12)


data("wage2")

wage2 %>% as_tibble() %>% 
  ggplot(aes(y = wage, x = hours)) +
  geom_point(size = 2, color = "brown1", alpha = 0.7) +
  labs(x = "Hours worked per week", y = "Monthy earnings",
       title = "Wage vs. hours worked") +
  theme_ipsum_rc() +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12)
