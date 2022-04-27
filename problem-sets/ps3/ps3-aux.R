library(tidyverse)
library(hrbrthemes)
library(ggmcmc)
library(rstan)
library(ggthemes)

theme_set(theme_bw())



#-----


options(mc.cores = parallel::detectCores())  ## using your computer cores.


rstan_options(auto_write = TRUE)   ## automatically saves your stan model.




#-----


model_5 <- "

  data {
  
  int<lower=0> n;
  int<lower=0, upper=n> Y;
  
  }
  
  parameters {
  
  real<lower=0, upper=1> theta;
  
  }
  
  model {
  
  Y ~ binomial(100, theta);
  theta ~ beta(3, 8);
  
  }
"


set.seed(123)    ## don't forget to set a seed!

model_5_mcmc <- stan(
  model_code = model_5,           ## the model from before.
  data = list(Y = 5, n = 100),    ## hyperparameters were defined above.
  chains = 4, iter = 5000 * 2     ## run 4 parallel Markov chains, with 10,000 simulations.
)



model_5_mcmc


model_5_tibble <- model_5_mcmc %>% ggs()

model_5_tibble %>% ggs_traceplot(greek = TRUE)


ggsave("trace.png")

model_5_tibble %>% ggs_density(greek = TRUE)

ggsave("density.png")



#---------


howell <- read_csv("howell.csv")


howell_adults <- howell %>% 
  filter(age > 18)


x_prior_height <- tibble(
  height_value = seq(0, 300, by = 0.1)
  
)


x_prior_height %>% 
  ggplot(aes(x = height_value)) +
  stat_function(fun = dnorm, args = list(mean = 170, sd = 15), size = 0.8)


170 - 2*15
170 + 2*15



#----


model_6 <- "

  data {

real<lower=0> Y[346];  // the filtered data set has 346 adults.

    
  }
  
  parameters {

real<lower=0> mu;   // we cannot have negative heights.

    
  }
  
  model {

Y ~ normal(mu, 15);   // sigma is assumed to be known here.
mu ~ normal(170, 15);
    
  }
  
"

set.seed(123)

model_6_mcmc <- stan(
  model_code = model_6,
  data = list(Y = howell_adults$height),
  chains = 4, 
  iter = 5000 * 2
)


model_6_mcmc


model_6_tibble <- model_6_mcmc %>% ggs()


model_6_tibble %>% ggs_traceplot(greek = TRUE)

ggsave("trace6.png")

model_6_tibble %>% ggs_density(greek = TRUE)

ggsave("density6.png")




#--------


model_7 <- "

  data {


real<lower=0> Y[544];  // using the whole sample.

    
  }
  
  parameters {

real<lower=0> mu;      // we cannot have negative weights.
real <lower=0, upper=20> sigma;
    
  }
  
  model {

Y ~ normal(mu, sigma);  
mu ~ normal(65, 10);      // assuming  ~ N(60,20) for the average weight.
sigma ~ uniform(0, 20);   // assuming the SD to be uniformly distributed between 0 and 20.
    
  }
  
"



set.seed(123)

model_7_mcmc <- stan(
  model_code = model_7,
  data = list(Y = howell$weight),
  chains = 4, 
  iter = 5000 * 2
)


model_7_mcmc


model_7_tibble <- model_7_mcmc %>% ggs()

model_7_tibble %>% ggs_traceplot(greek = TRUE)

ggsave("trace7.png")

model_7_tibble %>% ggs_density(greek = TRUE)

ggsave("density7.png")

