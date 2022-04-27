library(tidyverse)
library(janitor)
library(hrbrthemes)
library(AmesHousing)

theme_set(theme_ipsum())

ames_data <- ames_raw

ames_data <- ames_data %>% 
  clean_names()


# (a)

ames_data %>% 
  ggplot(aes(x = lot_area)) +
  geom_histogram(color = "white", binwidth = 8000)


# (b) 

ames_data %>% 
  summarize(mean_lot = mean(lot_area))


# (c)

lot <- ames_data %>% 
  pull(lot_area)


samples_50 <- rep(NA, 5000)

for(i in 1:5000){
  
  sampling_50 <- sample(lot, size = 50)
  
  samples_50[i] <- mean(sampling_50)
  
}


# (d)

samples_50 <- samples_50 %>% 
  as_tibble()


samples_50 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "white") +
  labs(title  = "Sampling distribution of the sample mean (n = 50)",
       x = "sample mean")


# (e)

samples_1000 <- rep(NA, 5000)

for(i in 1:5000){
  
  sampling_1000 <- sample(lot, size = 1000)
  
  samples_1000[i] <- mean(sampling_1000)
  
}


samples_1000 <- samples_1000 %>% 
  as_tibble()


samples_1000 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "white") +
  labs(title  = "Sampling distribution of the sample mean (n = 1,000)",
       x = "sample mean")


# (f)

ames_data %>% 
  summarize(mean_lot = mean(lot_area))

samples_50 %>% 
  summarize(mean_sampling50 = mean(value))

samples_1000 %>% 
  summarize(mean_sampling1000 = mean(value))


# (g)

ames_data %>% 
  summarize(var_lot = var(lot_area))

samples_50 %>% 
  summarize(var_sampling50 = var(value))

samples_1000 %>% 
  summarize(var_sampling1000 = var(value))



# (h)

ames_data %>% 
  summarize(median_lot = median(lot_area))


#-- sampling n = 50:

samples_50_median <- rep(NA, 5000)

for(i in 1:5000){
  
  sampling_50_median <- sample(lot, size = 50)
  
  samples_50_median[i] <- median(sampling_50_median)
  
}

samples_50_median <- samples_50_median %>% 
  as_tibble()


#-- sampling n = 1,000

samples_1000_median <- rep(NA, 5000)

for(i in 1:5000){
  
  sampling_1000_median <- sample(lot, size = 1000)
  
  samples_1000_median[i] <- median(sampling_1000_median)
  
}

samples_1000_median <- samples_1000_median %>% 
  as_tibble()


ames_data %>% 
  summarize(median_lot = median(lot_area))

samples_50_median %>% 
  summarize(median_samples50 = median(value))

samples_1000_median %>% 
  summarize(median_samples1000 = median(value))



#----------------------------------------------------------------------------


# Problem 2

# (a)

pnorm(q = 1, mean = 2, sd = 1)


# (b)

qnorm(p = 0.85, mean = 3, sd = 1)


# (c)

qnorm(p = 0.025, mean = 0, sd = 1)


# (d)

qnorm(p = 0.05, mean = 0, sd = 1)


# (e)

qnorm(p = 0.005, mean = 0, sd = 1)




min <- 0
max <- 1


ggplot(data.frame(x = c(min, max)), aes(x = x)) +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 8), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 8), size = 1.2, alpha = 0.8)
