library(tidyverse)
library(hrbrthemes)
library(AmesHousing)
library(janitor)
library(patchwork)
library(ggeasy)

theme_set(theme_ipsum_rc())


#---------------------------------------------------


ames <- ames_raw


ames <- ames %>% 
  clean_names()

ames %>% 
  select(gr_liv_area)


ames %>% 
  ggplot(aes(x = gr_liv_area)) +
  geom_histogram(binwidth = 200, color = "white", fill = "#bd969b", alpha = 0.8) +
  labs(x = "Above ground living area") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

ames %>% 
  summarize(pop_mean = mean(gr_liv_area),
            pop_variance = var(gr_liv_area),
            pop_sd = sd(gr_liv_area))



area <- ames %>% 
  pull(gr_liv_area)




sample_means50 <- rep(NA, 5000) 

for(i in 1:5000){    
  
  s50 <- sample(area, 50)
  
  sample_means50[i] <- mean(s50)
  
}


samples <- sample_means50 %>% 
  as_tibble() %>% 
  rename(mean_50 = value)



sample_means500 <- rep(NA, 5000) 

for(i in 1:5000){   
  
  s500 <- sample(area, 500)
  
  sample_means500[i] <- mean(s500)
  
}


sample_means500 <- sample_means500 %>% 
  as_tibble() %>% 
  rename(mean_500 = value)
 


samples <- samples %>% 
  add_column(sample_means500)


samples %>% 
  ggplot(aes(x = mean_50)) +
  geom_histogram(color = "white", fill = "#204864", alpha = 0.4) +
  geom_histogram(aes(x = mean_500), color = "white", fill = "#fa8072", alpha = 0.5)


#----------------------------------------------------------------------------------------


dice <- tibble(
  value = c(2, 3, 3, 4, 4, 4, 5, 5, 5, 5,
            6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7,
            8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10,
            11, 11, 12)
)


dice %>% 
  count(value, sort = TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = value, y = n)) +
  geom_col(fill = "#774291", alpha = 0.8)

dice %>% 
  count(value, sort = TRUE) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = n, y = pct)) +
  geom_col(fill = "#774291", alpha = 0.8)


die <- tibble(
  value = c(1, 2, 3, 4, 5, 6)
)


die %>% 
  count(value, sort = TRUE) %>% 
  ggplot(aes(x = value, y = n)) +
  geom_col(fill = "#c62460", alpha = 0.6)


dice_rolls <- tibble(
  mean = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6),
  prob_mean = c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)
)

dice_rolls %>% 
  mutate(exp_value = mean * prob_mean) %>% 
  summarize(expected_value = sum(exp_value))

dice_rolls %>% 
  ggplot(aes(x = mean, y = prob_mean)) +
  geom_col(fill = "#607f52", color = "white", alpha = 0.6) +
  labs(x = "Sample mean",
       y = "Probability")


#----------------------------------------------------------------------------------------



