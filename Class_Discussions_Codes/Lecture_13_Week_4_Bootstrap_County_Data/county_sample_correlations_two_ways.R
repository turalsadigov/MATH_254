## Load libraries and R data


library(tidyverse)
library(infer)
library(visdat)



county <- read_csv('https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/county.csv')
county

county %>% vis_dat()

county %>% vis_miss()



## Change the data format to tibble (advanced data frame) and view portion of it.

county_df <- 
  county %>% 
  select(-smoking_ban) %>% 
  drop_na() %>% 
  as_tibble(county)
county_df

## Scatterplot for unemployment vs poverty.

#| out.width = "100%"
county_df %>% 
  ggplot(aes(x = poverty, y = unemployment_rate)) +
  geom_point(fill = 'green', alpha = 0.5) +
  geom_smooth(method = 'lm')



## Polulation Correlation

county_df %>% 
  summarise(r = cor(poverty, unemployment_rate))

county_df %>% 
  specify(poverty ~ unemployment_rate) %>% 
  calculate(stat = 'correlation')

## Sample

set.seed(2345)
county_sample <- 
  county_df %>% 
  slice_sample(n = 100)
county_sample %>% 
  summarise(r = cor(poverty, unemployment_rate))

## Bootstrap distribution of sample correlation coefficient

set.seed(5678)
county_sample %>% 
  rep_sample_n(size = 100, reps = 1000, replace = TRUE) %>% 
  summarise(r = cor(poverty, unemployment_rate)) %>% 
  ggplot(aes(r)) +
  geom_density(fill = 'blue', alpha = 0.5) 

## 90% Bootstrap Percentila Confidence Interval

set.seed(5678)
county_sample %>% 
  rep_sample_n(size = 100, reps = 1000, replace = TRUE) %>% 
  summarise(r = cor(poverty, unemployment_rate)) %>% 
  summarise(ave_rs = mean(r), 
            left = quantile(r, 0.025),
            right = quantile(r, 0.975))



# infer way of doing
set.seed(5678)
ci <- 
  county_sample %>%
  specify(poverty ~ unemployment_rate) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "correlation") %>% 
  get_ci(level = .95)
ci

set.seed(5678)
county_sample %>%
  specify(poverty ~ unemployment_rate) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "correlation") %>% 
  visualize() +
  shade_confidence_interval(endpoints = ci)

set.seed(5678)
county_sample %>%
  specify(poverty ~ unemployment_rate) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "correlation") %>% 
  summary()
