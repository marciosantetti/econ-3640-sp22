library(tidyverse)
library(rstan)
library(hrbrthemes)
library(ggmcmc)
library(bayesrules)

theme_set(theme_ipsum_rc())

options(mc.cores = parallel::detectCores())


rstan_options(auto_write = TRUE)


howell <- read_csv("howell.csv")



height_model <- "

  data {

real theta;
real<lower=0> tau;
int alpha;
int beta;
real<lower=0> Y[544];

    
  }
  
  parameters {

real<lower=0> mu;
real <lower=0, upper=50> sigma;
    
  }
  
  model {

Y ~ normal(mu, sigma);
mu ~ normal(theta, tau);
sigma ~ uniform(alpha, beta);
    
  }
  
"


set.seed(123)

model_height <- stan(
  model_code = height_model,
  data = list(theta = 178, tau = 20, alpha = 0, beta = 50, Y = howell$height),
  chains = 4, iter = 5000 * 2
)


model_height_tibble <- model_height %>% ggs()

model_height_tibble %>% ggs_density()

model_height_tibble %>% 
  filter(Chain %in% c(1, 2)) %>% 
  filter(Parameter == "sigma") %>% 
  mutate(Chain = as_factor(Chain)) %>% 
  ggplot(aes(x = value, color = Chain)) +
  geom_density() +
  scale_color_viridis_d()




######




disasters <- c(4, 5, 4, 0, 1, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1,
4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3,
0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0,
0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2,
0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1)

years <- seq(from = 1852, to = 1962, by = 1)


coal_disasters <- tibble(
  disasters = c(4, 5, 4, 0, 1, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1,
                 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3,
                 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0,
                 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2,
                 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1),
  year = seq(from = 1852, to = 1962, by = 1)
)


coal_disasters %>% 
  ggplot(aes(x = year, y = disasters)) +
  geom_line() +
  geom_point()


coal_disasters %>% 
  write_csv("coal_mine_disasters.csv")


ggplot(data.frame(x = c(1, 40)), aes(x = x)) +
  stat_function(fun = dgamma, args = list(rate = 1, shape = 10), size = 0.7) +
  #scale_x_continuous(breaks = seq(0,100,10)) +
  labs(x = expression(lambda),
       y = expression(f(lambda)),
       title = "Gamma(1, 10)")


# Prior: lambda ~ Gamma(1,10)
# Likelihood: Y|lambda ~ Poisson(lambda)


coal_model <- "

  data {

real<lower=0> rate;
real<lower=0> shape;
int<lower=0> Y[111];
    
  }
  
  parameters {

real<lower=0> lambda;
    
  }
  
  model {

Y ~ poisson(lambda);
lambda ~ gamma(rate, shape);
    
  }
  
"


set.seed(123)

model_coal <- stan(
  model_code = coal_model,
  data = list(rate = 1, shape = 10, Y = coal_disasters$disasters),
  chains = 4, iter = 5000 * 2
)


model_coal %>% ggs() %>% 
  ggs_density()

model_coal %>% ggs() %>% 
  ggs_traceplot()

model_coal %>% ggs() %>% 
  ggs_autocorrelation()

summarize_gamma_poisson(shape = 10, rate = 1, sum_y = 191, n = 111)

plot_gamma_poisson(shape = 10, rate = 1, sum_y = 191, n = 111)

qgamma(p = c(0.025, 0.975), shape = 201, rate = 112)


model_coal %>% summary()


model_coal_tibble <- model_coal %>% ggs()

model_coal_tibble %>% 
  mutate(Chain = as_factor(Chain)) %>% 
  ggplot(aes(x = value, color = Chain)) +
  geom_density() +
  geom_vline(xintercept = mean(model_coal_tibble$value)) +
  geom_vline(xintercept = 1.36, lty = 2) +
  geom_vline(xintercept = 1.82, lty = 2)


model_coal_tibble %>% 
  summarize(mean(value),
            median(value))
