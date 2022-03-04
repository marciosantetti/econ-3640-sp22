

#== ECON 3640-001 -- Spring 2022
#== Marcio Santetti

#=============================================================================#
#             PROBABILITY CONCEPTS - DISCRETE RANDOM VARIABLES                #
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


## We will run some simulations, based on randomly generated data. R has some very useful functions
## for generating random data that follow specific probability distributions.

## We'll start with discrete distributions (Binomial and Poisson).


## Since we want to generate random numbers, we'll set a "seed" for reproducibility.

set.seed(123)


## Suppose we want run a survey with 50 people, asking them whether they are full-time workers or not.
## This way, let us define being a full-time worker as a "success" outcome, and not being a full-time
## worker as a "failure." Also, let us assume:

## P(success) = 0.5
## P(failure) = 1 - P(success) = 0.5


## The functions start with "r" prefix, followed by a distribution name, serve as a pseudo-random
## number generator:


full_time <- rbinom(n = 100, size = 50, prob = 0.5)  ## we're running the simulation 100 times.

full_time <- full_time %>% as_tibble()

full_time <- full_time %>% 
  rename(full_work = value)  ## renaming the column.


## First, a histogram:

full_time %>% 
  ggplot(aes(x = full_work)) +
  geom_histogram(color = "white", binwidth = 1)


## Changing the y-axis to relative frequencies:


full_time %>% 
  ggplot(aes(x = full_work)) +
  geom_histogram(aes(y = ..density..), color = "white", binwidth = 1)



## Let us calculate relative frequencies from the data:


full_time <- full_time %>% 
  count(full_work, sort=TRUE) %>% 
  mutate(prob = n/sum(n))

full_time %>% 
  ggplot(aes(y = prob, x = full_work)) +
  geom_point() +
  geom_segment(aes(x = full_work, xend = full_work, y = 0, yend = prob)) ## adding vertical lines.


## Now, let's plot the probability mass function (PMF) using ggplot2:
## For PMFs/PDFs, we use the "d" prefix:


full_time %>% 
  ggplot(aes(x = full_work)) +
  stat_function(fun = dbinom, args = list(size = 50, prob = 0.5), 
                geom = "area", fill = "#40e0d0", alpha = 0.7) +
  stat_function(fun = dbinom, args = list(size = 50, prob = 0.5), 
                color = "#40e0d0", size = 1, alpha = 0.5)   ## How different is it from the previous plot?


## Now, the two together:

full_time %>% 
  ggplot(aes(y = prob, x = full_work)) +
  geom_point() +
  geom_segment(aes(x = full_work, xend = full_work, y = 0, yend = prob)) +
  stat_function(fun = dbinom, args = list(size = 50, prob = 0.5), 
                geom = "area", fill = "#40e0d0", alpha = 0.7) +
  stat_function(fun = dbinom, args = list(size = 50, prob = 0.5), 
                color = "#40e0d0", size = 1, alpha = 0.5)


## Expected value and variance:

full_time %>% 
  summarize(expected_value = mean(full_work),
            variance  =  var(full_work),
            sd = sd(full_work))



##== Calculating specific probabilities [P(X = x)]:



# What is the probability of 30 successes in this experiment? i.e., P(X = 30).


dbinom(x = 30, size = 50, prob = 0.5)   ## The R way.

choose(n = 50, k = 30) * (0.5)^30 * (1 - 0.5)^(50 - 30)  ## Using the formula for the Binomial distribution.


## Calculating cumulative probabilities (CDF) [P(X < x)]:


# What is the probability of 25 or less successes in this experiment? i.e., P(X < 25).


pbinom(q = 25, size = 50, prob = 0.5)


## Calculating percentiles from the probability distribution:

qbinom(p = 0.55, size = 50, prob = 0.5)   ## this is also known as the "inverse CDF."


#-------------------------------------------------------------------------------------------



## Poisson distribution:


## Let's model the number of spam messages arriving in your email during a 7-day week.

set.seed(123)


spam_messages <- rpois(n = 100, lambda = 2)   ## the average (lambda) is 2.


spam_messages <- spam_messages %>% 
  as_tibble() %>% 
  rename(spam = value)


spam_messages <- spam_messages %>% 
  count(spam, sort=TRUE) %>% 
  mutate(prob = n/sum(n))

spam_messages %>% 
  ggplot(aes(y = prob, x = spam)) +
  geom_point() +
  geom_segment(aes(x = spam, xend = spam, y = 0, yend = prob))


spam_messages %>% 
  ggplot(aes(x = spam)) +
  stat_function(fun = dpois, args = list(lambda = 2), 
                geom = "area", fill = "#40e0d0", alpha = 0.7) +
  stat_function(fun = dpois, args = list(lambda = 2), 
                color = "#40e0d0", size = 1, alpha = 0.5)


spam_messages %>% 
  ggplot(aes(y = prob, x = spam)) +
  geom_point() +
  geom_segment(aes(x = spam, xend = spam, y = 0, yend = prob)) +
  stat_function(fun = dpois, args = list(lambda = 2), 
                geom = "area", fill = "#40e0d0", alpha = 0.7) +
  stat_function(fun = dpois, args = list(lambda = 2), 
                color = "#40e0d0", size = 1, alpha = 0.5)



## Expected value and variance:

spam_messages %>% 
  summarize(expected_value = mean(spam),
            variance = var(spam),
            sd = sd(spam))



##== Calculating specific probabilities [P(X = x)]:



# What is the probability of receiving 3 spam messages in random week? i.e., P(X = 3).


dpois(x = 3, lambda = 2)    ## The R way.

exp(-2) * (-2)^3 / factorial(3)   ## Using the formula.


#-------------------------------------------------------------------------------------------


