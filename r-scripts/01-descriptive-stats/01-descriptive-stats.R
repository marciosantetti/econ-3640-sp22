

#== ECON 3640-001 -- Spring 2022
#== Marcio Santetti

#=============================================================================#
#                 DESCRIPTIVE STATISTICS I - DATA VISUALIZATION               #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files.' Select your desired folder, and click on 'More,' then select 
# the option 'Set as Working Directory.'


#==============================================================================#


library(tidyverse)
library(scales)
library(ggthemes)
library(hrbrthemes)
library(skimr)
library(patchwork)
library(ggeasy)



#==============================================================================#



# article: https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/


majors <- read_csv("college-majors.csv")


majors %>% View()



##=== Bar chart:



majors %>% 
  count(Major_category, sort=TRUE) %>% 
  mutate(Major_category = fct_reorder(Major_category, n)) %>% 
  ggplot(aes(x = n, y = Major_category)) +
  geom_col(color = "white", fill = "turquoise", alpha = 0.7) +
  labs(x = "Number of distinct majors", y = "College major", 
       title = "Students enrolled by category",
       subtitle = "Nice bar plot",
       caption = "Data from fivethirtyeight.com") +
  theme_ipsum()


#-----

## Choosing theme_ipsum as the default plotting theme:

theme_set(theme_ipsum())

majors %>% 
  group_by(Major_category) %>% 
  filter(!is.na(ShareWomen)) %>% # "filter" function filters by row.
  summarize(mean = mean(ShareWomen)) %>% 
  arrange(desc(mean)) %>% 
  ggplot(aes(x = mean, y = fct_reorder(Major_category, mean))) +
  geom_col(color = "white")



##=== Pie chart:

states <- tibble(
  state = c(rep("Utah", 2), rep("California", 4), 
            rep("Ohio",2), rep("Other", 3), "Pennsylvania", rep("Illinois",2), rep("Florida",2))
)



states %>% 
  count(state, sort = TRUE) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot() +
  geom_col(aes(x = n, y = state), fill = "#3c0c2c", alpha = 0.4)


states_pie <- states %>% 
  count(state, sort = TRUE) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  mutate(share_state = 100*n/sum(n)) %>% 
  mutate(share_state_pct = n/sum(n))

states_pie %>%
  ggplot() +
  geom_col(aes(x = share_state_pct, y = state)) +
  scale_x_continuous(labels = percent_format())

states_pie %>%
  ggplot() +
  geom_col(aes(x = share_state, y = state)) +
  scale_x_continuous(labels = percent_format(scale = 1))


with(states_pie,
     pie(n,
         radius = 1, 
         density = 30,
         labels = paste0(state,": ", share_state, "%"),
         col = c("#000000", "#773c3c", "#d02525", "#e69138", "#dfb283", "#4d942f", "#d31515"),
         main = "A pie chart"))



###

coffee <- tibble(
  answer = c(rep("Yes", 10), rep("No", 7))
)

coffee_pie <- coffee %>% 
  count(answer, sort=TRUE) %>% 
  mutate(pct = n/sum(n) * 100)

with(coffee_pie,
     pie(pct,
         radius = 1, 
         density = 30,
         labels = paste0(answer, ": ", round(pct,2), "%"),
         col = c("#000000", "#773c3c"),
         main = "Coffe drinkers?"
         ))



##=== Histogram:


netflix <- read_csv("netflix.csv")


netflix %>% 
  select(type, duration) %>% 
  head(20)

netflix %>% 
  ggplot(aes(x = release_year)) +
  geom_histogram(color = "white", fill = "#74867c", binwidth = 10, alpha = 0.7)

netflix %>% 
  count(type)


netflix %>% 
  ggplot(aes(x = release_year, fill = type)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_fill_wsj()


## We can store plots created with ggplot2 in R objects, as below:


p1 <- netflix %>% 
  filter(type == "Movie") %>% 
  ggplot(aes(x = release_year)) +
  geom_histogram(fill = "black", color = "white") +
  labs(title = "Netflix movies: release years")

p2 <- netflix %>% 
  filter(type == "TV Show") %>% 
  ggplot(aes(x = release_year)) +
  geom_histogram(fill = "firebrick", color = "white") +
  labs(title = "Netflix TV shows: release years")


p1 / p2 ## using the 'patchwork' package.


## Another option is using "facet_wrap," as below:

netflix %>% 
  ggplot(aes(release_year, fill = type)) +
  scale_fill_ft() +
  geom_histogram(color = "white") +
  facet_wrap(~ type, ncol = 1, scales = "free_y") 



##=== Scatter plot:

majors %>% 
  group_by(Major_category) %>% 
  filter(!is.na(ShareWomen)) %>%
  ggplot(aes(x = ShareWomen, y = Unemployment_rate)) +
  geom_point(color = "#a83d3d", alpha = 0.7) 



##=== Box plot:

netflix %>% 
  filter(type == "Movie",
         ! rating %in% NA) %>% 
  separate(duration, c("duration_mins", "just minutes")) %>% ## separating the "duration" column.
  mutate(duration_mins = as.integer(duration_mins), ## transforming "mins" into an integer value.
         rating = fct_reorder(rating, duration_mins)) %>% 
  ggplot(aes(x = duration_mins, y = rating)) +
  geom_boxplot(color = "#6c2bac") +
  labs(x = "Duration (mins)",
       y = "Movie Ratings",
       title = "Netflix movie durations (in minutes)") +
  easy_x_axis_title_size(12) +
  easy_y_axis_title_size(12) +
  scale_x_continuous(breaks = seq(0,350,50))




