

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
library(ggmcmc)
library(rstan)

theme_set(theme_ipsum())



#======================================================================================#



#------------ A Gamma-Poisson model:


## Let us first model the number of coal mining disasters per year in the UK.
## The data set below contains data between 1852 and 1962:



coal_disasters <- read_csv("coal_mine_disasters.csv")



## The number of disasters is a discrete, count variable. 
## Thus, we can easily model our data through a Poisson distribution.

## Y ~ Poisson(lambda)

## We need to set a prior distribution for our parameter of interest, lambda.

## By conjugacy, we may use the Gamma distribution:

## lambda ~ Gamma(rate, shape)

## The prior must be set before seeing any data. THe Gamma distribution has two
## hyperparameters: shape and rate.

## Let us play around with some possible values for them:

x_prior <- tibble(
  x_value = seq(0, 100, by = 1)
)


# lambda ~ Gamma(1, 10):

x_prior %>% 
  ggplot(aes(x = x_value)) +
  stat_function(fun = dgamma, args = list(rate = 1, shape = 10), size = 0.7)

# lambda ~ Gamma(1, 50):

x_prior %>% 
  ggplot(aes(x = x_value)) +
  stat_function(fun = dgamma, args = list(rate = 1, shape = 50), size = 0.7)

# lambda ~ Gamma(2, 20):

x_prior %>% 
  ggplot(aes(x = x_value)) +
  stat_function(fun = dgamma, args = list(rate = 2, shape = 20), size = 0.7)


# lambda ~ Gamma(5, 50):

x_prior %>% 
  ggplot(aes(x = x_value)) +
  stat_function(fun = dgamma, args = list(rate = 5, shape = 50), size = 0.7)


coal_disasters %>% 
  ggplot(aes(x = disasters)) +
  geom_histogram(binwidth = 1, color = "white")

coal_disasters %>% 
  ggplot(aes(x = year, y = disasters)) +
  geom_point()



## Let's assume we set our prior to lambda ~ Gamma(1, 10).


## Then, let's build our model using rstan:


options(mc.cores = parallel::detectCores())  ## using your computer cores.


rstan_options(auto_write = TRUE)   ## automatically saves your stan model.




##-- Defining the model:

coal_model <- "

  data {

real<lower=0> rate;       // define your hyperparameters first!
real<lower=0> shape;     // define your hyperparameters first!
int<lower=0> Y[111];    // the data vector contains 111 observations.
    
  }
  
  parameters {

real<lower=0> lambda;
    
  }
  
  model {

Y ~ poisson(lambda);
lambda ~ gamma(rate, shape);
    
  }
  
"


##-- Running the MCMC sampler:


set.seed(123)   ## Don't forget to set a seed!

model_coal <- stan(
  model_code = coal_model,
  data = list(rate = 1, 
              shape = 10, 
              Y = coal_disasters$disasters), ## selecting the "disasters" column.
  chains = 4, 
  iter = 5000 * 2
)


## Now, the object "model_coal" contains our MCMC estimation for the posterior distribution of lambda,
## the rate of coal mining disasters per year.


## First, let us summarize our model:

model_coal   ## Notice the summary statistics!


## Given our prior and data, the posterior mean is of 1.59 disasters/year.
## Furthermore, given our data and priors, about 95% of our posterior observations
## lie between 1.37 and 1.82 disasters/year.


## Let us look at how the Markov chains behaved:

model_coal_tibble <- model_coal %>% ggs()  ## The "ggs()" function from the "ggmcmc" package
                                           ## converts the MCMC estimation into a tibble, 
                                           ## which we can easily manipulate,



# A trace plot:

model_coal_tibble %>% ggs_traceplot()
model_coal_tibble %>% ggs_traceplot(greek = TRUE)  ## If you want to be fancy.


# Now, the posterior distribution:

model_coal_tibble %>% ggs_density(greek = TRUE)   ## posterior density for each Markov chain. 



# If we want to have more autonomy over the results, we can produce the plot above ourselves:

model_coal_tibble %>% 
  mutate(Chain = as_factor(Chain)) %>% ## converting the chain number into a factor.
  ggplot(aes(x = value)) +
  geom_density(aes(color = Chain)) +
  geom_vline(xintercept = 1.59) + ## including the posterior mean.
  geom_vline(xintercept = 1.37, lty = 2) + # including the 95% credibility interval.
  geom_vline(xintercept = 1.82, lty = 2)   # including the 95% credibility interval.



#------------------------------------------------------------------------------------


#------------ A Normal-Normal model:


## Let us estimate a Bayesian model for heights. 
## We'll use the "howell.csv" data set once again.


howell <- read_csv("howell.csv")



## Setting a prior for heights:

x_prior_height <- tibble(
  height_value = seq(0, 300, by = 0.1)
  
)


x_prior_height %>% 
  ggplot(aes(x = height_value)) +
  stat_function(fun = dnorm, args = list(mean = 178, sd = 20), size = 0.8)



## Defining the model:

height_model <- "

  data {

real theta;
real<lower=0> tau;
int alpha;
int beta;
real<lower=0> Y[544];  // we have 544 observations

    
  }
  
  parameters {

real<lower=0> mu;
real <lower=0, upper=50> sigma;
    
  }
  
  model {

Y ~ normal(mu, sigma);   // the Normal distribution has 2 parameters: mean and SD
mu ~ normal(theta, tau);
sigma ~ uniform(alpha, beta);
    
  }
  
"


## Running the sampler:

set.seed(123)

model_height <- stan(
  model_code = height_model,
  data = list(theta = 178, 
              tau = 20, 
              alpha = 0, 
              beta = 50,
              Y = howell$height),
  chains = 4, 
  iter = 5000 * 2
)


## Summary:

model_height    ## How do you interpret this?



model_height_tibble <- model_height %>% ggs()


model_height_tibble %>% ggs_traceplot()

model_height_tibble %>% ggs_density()


## Now, we have 2 parameters:


# mu:

model_height_tibble %>% 
  filter(Parameter == "mu") %>% 
  mutate(Chain = as_factor(Chain)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(color = Chain))
  

# sigma:

model_height_tibble %>% 
  filter(Parameter == "sigma") %>% 
  mutate(Chain = as_factor(Chain)) %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(color = Chain))
