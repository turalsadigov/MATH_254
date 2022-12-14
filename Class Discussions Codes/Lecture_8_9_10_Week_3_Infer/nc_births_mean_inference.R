# library
library(tidymodels)

# data
nc <- read_csv(file = 'https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/ncbirths.csv')

# summary
nc %>% 
  select(weeks) %>% 
  summary()


# distribution
nc %>% 
  ggplot(aes(weeks)) +
  geom_density(fill = 'darkgreen', alpha = 0.7)

nc %>% 
  ggplot(aes(x = weeks, y = ..density..)) +
  geom_histogram(fill = 'red', 
                 bins = 20, alpha = 0.5) +
  geom_density(fill = 'darkgreen', alpha = 0.7)

# cleaning
nc <- 
  nc %>% 
  drop_na()

# observed average weeks of pregnancy
obs_mean <- nc %>% 
  specify(response = weeks) %>% 
  calculate(stat = 'mean')
obs_mean

# observed test statistic
obs_test_stat <- nc %>% 
  specify(response = weeks) %>% 
  hypothesize(null = 'point', 
              mu = 39) %>% 
  calculate(stat = 't')

obs_test_stat

# null distribution
dist <- 
  nc %>% 
  specify(response = weeks) %>% 
  hypothesize(null = 'point', 
              mu = 39) %>% 
  assume(distribution = 't')

dist

dist %>% 
  visualize()


# get-p-value
dist %>%
  get_p_value(obs_stat = obs_test_stat, 
              direction = "both")

# shade the p-value in two sided test

dist %>% 
  visualize(method = 'theoretical') +
  shade_p_value(obs_stat = obs_test_stat, 
                direction = 'both')



# get - 99% confidence intervals

dist %>% 
  get_confidence_interval(
    point_estimate = obs_mean,
    level = 0.99,
    type = "se"
  )



