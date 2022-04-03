library(tidyverse)
library(rstan)
library(bayesplot)
library(hrbrthemes)
library(coda)
library(ggmcmc)
library(bayesrules)

theme_set(theme_ipsum_rc())

bayesplot_theme_set(theme_ipsum_rc())


options(mc.cores = parallel::detectCores())


## Save rstan model:

rstan_options(auto_write = TRUE)

# defining a model:

model <- "

data {

real<lower=0> beta;
real<lower=0> alpha;
int<lower=1> n;
int<lower=0, upper=n> Y;

}


parameters {

real<lower=0, upper=1> theta;

}


model {

Y ~ binomial(n, theta);
theta ~ beta(alpha, beta);



}



"


# simulating the posterior:

set.seed(123)

model_sim <- stan(
  model_code = model,
  data = list(alpha = 20, beta = 60, Y = 30, n = 100),
  chains = 4, iter = 5000 * 2
)


model_sim %>% 
  mcmc_trace(pars = "theta")

trace <- model_sim %>% 
  mcmc_trace(pars = "theta")

ggsave("trace.pdf")


model_sim %>% 
  mcmc_dens(pars = "theta")   # combined values for all chains.


dat <- ggs(model_sim)


dat %>% 
  ggs_histogram(greek = TRUE) 


model_chains <- as.data.frame(model_sim,
                              pars = "lp__",
                              include = FALSE)

model_chains %>% 
  as_tibble() %>% 
  summarize(mean(theta),
            sample_mode(theta),
            median(theta))


post_model <- model_sim %>% ggs()


post_model %>% summarize(mean_theta = mean(value))


post_model %>% ggs_density()

post_model %>% ggs_traceplot()

#------------------------------------------------------------------------------------

## Grid approximation:

data_grid <- tibble(
  theta_grid = seq(from = 0, to = 1, by = 0.01),
  prior = dbeta(theta_grid, shape1 = 2, shape2 = 2),
  likelihood = dbinom(x = 9, size = 10, prob = theta_grid)
)

data_grid <- data_grid %>% 
  mutate(unstd_posterior = prior * likelihood,
         std_posterior = unstd_posterior/sum(unstd_posterior))

data_grid %>% 
  summarize(sum_posterior = sum(std_posterior),
            sum_prior = sum(prior),
            sum_lik = sum(likelihood))


data_grid %>% 
  ggplot(aes(x = theta_grid, y = std_posterior)) +
  geom_line() 


data_grid %>% 
  ggplot(aes(x = theta_grid, y = prior)) +
  geom_line()


data_grid %>% 
  ggplot(aes(x = theta_grid, y = likelihood)) +
  geom_line()





#----


set.seed(123)

dd <- tibble(
  lambda_par = rgamma(10000, shape = 500, rate = 2),
  y = rpois(10000, lambda = lambda_par)
  
  
)


dd %>% 
  filter(y == 250) %>% 
  nrow()

dd %>% 
  filter(y == 250) %>% 
  ggplot(aes(x = lambda_par)) +
  geom_density() +
  stat_function(fun = dgamma, args = list(shape = 750, rate = 3), color = "red")
