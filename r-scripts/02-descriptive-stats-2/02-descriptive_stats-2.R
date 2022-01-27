

#== ECON 3640-001 -- Spring 2022
#== Marcio Santetti

#=============================================================================#
#                 DESCRIPTIVE STATISTICS I - DATA VISUALIZATION               #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files.' Select your desired folder, and click on 'More,' then select 
# the option 'Set as Working Directory.'


#==============================================================================#


library(tidyverse)
library(hrbrthemes)
library(skimr)
library(ggrepel)



#==============================================================================#


##=== Analyzing drug overdose and poverty & unemployment data

# Sources: https://ukcpr.org/resources/national-welfare-data
#          https://www.cdc.gov/opioids/data/index.html


## Thanks to Kyle Raze for making the data available.


##------------------------------------------------------------------


theme_set(theme_ipsum())

drug_data <- read_csv("cdc_data.csv")


#--------------------------------------------------------------------



##=== Univariate descriptive techniques:


drug_data %>% skim()    ## a general overview of the data set (using the "skimr" package).

drug_data <- drug_data %>% 
  mutate(death_per100k = (deaths/population) * 100000)  ## creating a new variable, death_per100k.


drug_data %>% 
  count(year)   # checking what years we have...


## We can use the "summarize()" function (part of the tidyverse) to calculate summary statistics:


drug_data17 <- drug_data %>% 
  filter(year == "2017")         ## filtering out just observations for 2017.


## Manual calculations:


drug_data17 %>% 
  summarize(mean_deathrate = sum(death_per100k) / nrow(drug_data17),
            var_deathrate = (1/(nrow(drug_data17)-1)) * (sum(death_per100k^2) - sum(death_per100k)^2 / nrow(drug_data17)),
            sd_deathrate = sqrt(var_deathrate),
            range_deathrate = max(death_per100k) - min(death_per100k),
            q2_deathrate = quantile(death_per100k, p = 0.5))



## Now, using the R built-in functions:


drug_data17 %>% 
  summarize(mean_deathrate = mean(death_per100k),
            median_deathrate = median(death_per100k),
            variance_deathrate = var(death_per100k),
            sd_deathrate = sd(death_per100k))




##  Which state showed the minimum death rate? Which state showed the maximum?
##  The 'slice()' function helps us with that.


drug_data %>% slice(which.min(death_per100k))   

drug_data %>% slice(which.max(death_per100k))

#--------------------------------------------------------------------




##=== Bivariate descriptive techniques:



## Manual computation:


drug_data17 %>% 
  summarize(cov_deaths_unem = (1/(nrow(drug_data17) - 1)) * (sum(death_per100k * unemployment_rate) - (sum(death_per100k) * sum(unemployment_rate))/nrow(drug_data17)),
            cor_deaths_unem = cov_deaths_unem/(sd(death_per100k) * sd(unemployment_rate)),
            r2_deaths_unem = cor_deaths_unem^2 * 100)




## Now, using the R built-in functions:


drug_data17 %>% 
  summarize(cov_deaths_unem = cov(death_per100k, unemployment_rate),
            cor_deaths_unem = cor(death_per100k, unemployment_rate),
            r2_deaths_unem = cor(death_per100k, unemployment_rate)^2 * 100)





