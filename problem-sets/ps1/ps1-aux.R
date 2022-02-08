library(tidyverse)
library(wooldridge)
library(hrbrthemes)
library(scales)
library(RcppRoll)
library(lubridate)
library(zoo)

theme_set(theme_ipsum_rc())


data("mroz")

mroz_tibble <- mroz %>% as_tibble()


mroz_tibble %>% 
  count(educ, sort=TRUE)

mroz_tibble %>% 
  select(educ, wage) %>% 
  head(5) %>% 
  summarize(sum_educ = sum(educ),
            sum_wage = sum(wage),
            sd_educ = sd(educ),
            sd_wage = sd(wage),
            covariance = cov(educ, wage),
            correlation = cor(educ, wage),
            r_squared = cor(educ, wage)^2 * 100)

mroz_tibble %>% 
  mutate(inlf = as_factor(inlf)) %>% 
  ggplot(aes(x = huswage, fill = inlf)) +
  geom_histogram(color = "white") +
  scale_fill_ft() 

mroz_tibble <- mroz_tibble %>% 
  mutate(inlf = as_factor(inlf))
  
mroz_tibble %>% 
  ggplot(aes(x = huswage)) +
  geom_histogram(color = "white") +
  facet_wrap(~inlf, ncol = 1) 



####


change <- tibble(
  how_much = c(52, 25, 15, 0, 104, 44, 60, 30, 33, 81, 40, 5)
)

change %>% 
  count(how_much) # putting values in ascending order.

change %>% 
  summarize(summ = sum(how_much),
            var_hm = var(how_much),
            sd_hm = sd(how_much)) %>% View()

mroz_tibble %>% 
  ggplot(aes(x = huswage, y = faminc, color = inlf)) +
  geom_point()


mroz_tibble %>% 
  count(inlf, sort=TRUE) %>% 
  mutate(lf_share = round(n/sum(n), 2)) %>% 
  ggplot(aes(y = inlf, x = lf_share)) +
  geom_col(color = "black", alpha = 0.7) +
  scale_x_continuous(labels = percent_format())
  



#####


covid_cases <- read_csv("covid-cases-22.csv")

covid_cases <- covid_cases %>% 
  mutate(period = mdy(period))

covid_cases %>% 
  ggplot(aes(x = period, y = new_cases)) +
  geom_line() +
  scale_y_continuous(labels = comma)

covid_cases %>% 
  mutate(rollavg = rollmean(new_cases, k = 14, align = "right", fill = NA))



covid_cases %>% 
  mutate(new_cases_roll = roll_mean(new_cases, n = 14, align = "right", fill = NA),
         rollavg = rollmean(new_cases, k = 14, align = "right", fill = NA)) %>% 
  ggplot(aes(x = period, y = new_cases_roll)) +
  geom_line() +
  scale_y_continuous(labels = comma)

