

#== ECON 3640-001 -- Spring 2022
#== Marcio Santetti

#=============================================================================#
#             PROBABILITY CONCEPTS - CONTINUOUS RANDOM VARIABLES              #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files.' Select your desired folder, and click on 'More,' then select 
# the option 'Set as Working Directory.'


#==============================================================================#


library(tidyverse)
library(hrbrthemes)



theme_set(theme_ipsum())


#==============================================================================#


##== Let us start with the uniform (aka rectangular) distribution, in its continuous form.

## Suppose we are curious about figuring out the exact hour a person was born.
## If we assume there is an equal chance that they were born at any time of
## a day, we can model this situation through a uniform distribution.


## Let us generate some random numbers. We start by setting a seed for reproducibility:


set.seed(123)




hour_born <- runif(n = 100, min = 0, max = 24)   ## X ~ Unif(0, 24).


hour_born <- hour_born %>% as_tibble()


hour_born <- hour_born %>% 
  rename(hour = value)



## A histogram:


hour_born %>% 
  ggplot(aes(x = hour)) +
  geom_histogram(color = "white", binwidth = 0.5)


## Including the PDF of these simulated data:

hour_born %>% 
  ggplot(aes(x = hour)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#d5a6bd", binwidth = 0.5) +
  geom_density(size = 1)


## Summary statistics:

hour_born %>% 
  summarize(expected_value = mean(hour),
            variance = var(hour),
            sd = sd(hour))          ## Are these the same as the long-run expected values?



## And including the "long-run" PDF:


hour_born %>% 
  ggplot(aes(x = hour)) +
  geom_density(fill = "firebrick", alpha = 0.2, size = 1) +
  stat_function(fun = dunif, args = list(min = 0, max = 24), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dunif, args = list(min = 0, max = 24), size = 1, alpha = 0.5) 
  

## Some further questions:

# 1. What is the probability that the hour a random person was born lies between 10:00 am and 12:00 pm?

punif(q = 12, min = 0, max = 24) - punif(q = 10, min = 0, max = 24)


# 2. Looking at this visually...

hour_born %>% 
  ggplot(aes(x = hour)) +
  stat_function(fun = dunif, args = list(min = 0, max = 24), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dunif, args = list(min = 0, max = 24), size = 1, alpha = 0.5) +
  geom_area(stat = "function", fun = dunif, args = list(min = 0, max = 24),
            fill = "#bb4a54", xlim = c(10, 12), alpha = 0.5)




#-------------------------------------------------------------------------------------------


##== Now, to the Normal distribution:


## Suppose we want to model the temperature (in Celsius) in Utah for February. We set an average temperature 
## value (mean), allowing it to vary (variance/SD).



set.seed(123)


temperature <- tibble(
  temp = rnorm(1000, mean = 7, sd = 6)
)



## A histogram and density curve:


temperature %>% 
  ggplot(aes(x = temp)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#66b2b2", alpha = 0.6) +
  geom_density(size = 1)



## Summary statistics:


temperature %>% 
  summarize(mean_temp = mean(temp),
            var_temp = var(temp),
            sd_temp = sd(temp))




## Now, let us apply the properties of Expected Value and Variance studied in class.
## Recall ("c" is a constant):


# 1. EV(c) = c
# 2. EV(X + c) = EV(X) + c
# 3. EV(cX) = cEV(X)

# 1. Var(c) = 0
# 2. Var(c + X) = Var(X)
# 3. Var(cX) = c^2 * Var(X)
 

## And let's assess these properties in practice.
## Suppose we add 32 (a constant) to our original temperature data:



temperature <- temperature %>% 
  mutate(temp32 = temp + 32)


temperature %>% 
  summarize(mean_temp32 = mean(temp32),
            var_temp32 = var(temp32),
            sd_temp32 = sd(temp32))    ## What happened here?



## Histogram and density curve:

temperature %>% 
  ggplot(aes(x = temp32)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#e16fa2", alpha = 0.6) +
  geom_density(size = 1)


## Now, let's multiply our original temperature variable by a constant, 9/5:


temperature <- temperature %>% 
  mutate(temp3 = temp * 9/5)


temperature %>% 
  summarize(mean_temp3 = mean(temp3),
            var_temp3 = var(temp3),
            sd_temp3 = sd(temp3))  ## What happened here?


## Histogram and density curve:

temperature %>% 
  ggplot(aes(x = temp3)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#e16fa2", alpha = 0.6) +
  geom_density(size = 1)


## Now, let's put all of these pieces together:

temperature <- temperature %>% 
  mutate(temp_fahr = temp * 9/5 + 32)


temperature %>% 
  summarize(mean_temp_fahr = mean(temp_fahr),
            var_temp_fahr = var(temp_fahr),
            sd_temp_fahr = sd(temp_fahr))


## Histogram and density curve:

temperature %>% 
  ggplot(aes(x = temp_fahr)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#e16fa2", alpha = 0.6) +
  geom_density(size = 1)



## Plotting the simulated and long-run PDFs:


temperature %>% 
  ggplot(aes(x = temp)) +
  geom_density(color  = "red", size = 0.8) +
  stat_function(fun = dnorm, args = list(mean = 7, sd = 6), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 7, sd = 6), size = 1, alpha = 0.5)




## Some questions:


# 1. What is the probability that the temperature falls between 0 and 10 degrees Celsius?

pnorm(q = 10, mean = 7, sd = 6) - pnorm(q = 0, mean = 7, sd = 6)


# Visually...


temperature %>% 
  ggplot(aes(x = temp)) +
  stat_function(fun = dnorm, args = list(mean = 7, sd = 6), geom = "area", fill = "#8b9dc3", alpha = 0.35) +
  stat_function(fun = dnorm, args = list(mean = 7, sd = 6), size = 1, alpha = 0.5) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 7, sd = 6), 
            fill = "#b70673", xlim = c(0, 10), alpha = 0.4)




## A little challenge:

# Plot the simulated and long-run PDFs for the temperature in Fahrenheit (in the same plot):

#-------------------------------------------------------------------------------------------





