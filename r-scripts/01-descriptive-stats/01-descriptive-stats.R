library(tidyverse)
library(scales)
library(ggthemes)
library(hrbrthemes)


# article: https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/


majors <- read_csv("college-majors.csv")


majors %>% View()

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


####

theme_set(theme_ipsum())

majors %>% 
  group_by(Major_category) %>% 
  filter(!is.na(ShareWomen)) %>% 
  summarize(mean = mean(ShareWomen)) %>% 
  arrange(desc(mean)) %>% 
  ggplot(aes(x = mean, y = fct_reorder(Major_category, mean))) +
  geom_col(color = "white")
  


####






