library(tidyverse)
library(rstan)
library(hrbrthemes)
library(ggmcmc)
library(bayesrules)

theme_set(theme_ipsum_rc())

options(mc.cores = parallel::detectCores())


rstan_options(auto_write = TRUE)


gp_model <- "

  data {


real<lower=0> rate;
real<lower=0> shape;
int<lower=0> Y[10];
    
  }
  
  parameters {

real<lower=0> lambda;
    
  }
  
  model {

Y ~ poisson(lambda);
lambda ~ gamma(shape, rate);

    
  }
  
"

set.seed(123)

model_gp <- stan(
  model_code = gp_model,
  data = list(shape = 6, rate = 1, Y = c(6, 10, 3, 5, 7, 6, 6, 10, 3, 5)),
  chains = 4, iter = 5000 * 2
)

model_gp


model_tidy <- model_gp %>% ggs()


model_tidy %>% ggs_traceplot()

model_tidy %>% ggs_density(hpd = TRUE)



###------------------------------------------


## Normal-Normal model:

nn_model <- "

  data {

real theta;
real<lower=0> tau;
real Y[4];
    
  }
  
  parameters {

real mu;
    
  }
  
  model {

Y ~ normal(mu, 1.3);
mu ~ normal(theta, tau);
    
  }
  
"


set.seed(123)

model_nn <- stan(
  model_code = nn_model,
  data = list(theta = 10, tau = 1.2, Y = c(7.1, 8.9, 8.4, 8.6)),
  chains = 4, iter = 5000 * 2
)


model_nn_tibble <- model_nn %>% ggs()


model_nn_tibble %>% ggs_traceplot()

model_nn_tibble %>% ggs_density(greek = TRUE)



summarize_normal_normal(mean = 10, sd = 1.2, sigma = 1.3, y_bar = 8.25, n = 4)
