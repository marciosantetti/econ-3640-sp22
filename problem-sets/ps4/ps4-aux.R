library(tidyverse)
library(AmesHousing)
library(janitor)
library(hrbrthemes)
library(skimr)


theme_set(theme_ipsum())


#------------------------------------------------------------------------

#-------- Problem 1:


# (a)


ames_data <- ames_raw

ames_data <- ames_data %>% 
  clean_names()


ames_data %>% 
  summarize(mean_lot = mean(lot_area))


lot <- ames_data %>% 
  pull(lot_area)


samples_50 <- rep(NA, 5000)

for(i in 1:5000){
  
  s50 <- sample(lot, 50)
  samples_50[i] <- mean(s50)
  
  
}


samples_50 %>% 
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "white", binwidth = 500)

samples_50 %>% 
  as_tibble() %>% 
  summarize(mean = mean(value),
            var= var(value))


samples_500 <- rep(NA, 5000)

for(i in 1:5000){
  
  s500 <- sample(lot, 500)
  samples_500[i] <- mean(s500)
  
  
}


samples_500 %>% 
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "white", binwidth = 200)

samples_500 %>% 
  as_tibble() %>% 
  summarize(mean = mean(value),
            var = var(value))




#-------------------------------------------------------------------


set <- tibble(
  marks = c(7,9,7,5,4,8,3,10,9)
)


set %>% 
  summarize(mean = mean(marks),
            sd = sd(marks))
