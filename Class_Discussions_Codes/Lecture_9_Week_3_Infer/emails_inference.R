# install.packages('tidymodels')
library(tidymodels)

# data
emails <- read.csv('https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/email.csv')
emails <- 
  emails %>% 
  mutate(spam = as.factor(spam), 
         number = as.factor(number),
         winner = as.factor(winner)) %>% 
  drop_na() %>% 
  as_tibble()


# BASE R
summary(emails$line_breaks)

emails %>% 
  pull(line_breaks) %>% 
  summary()

# ======================== SPAM vs NUMBER =====================================
# null hypothesis and null distribution
dist <- 
  emails %>% 
  specify(spam ~ number, success = '1') %>% 
  hypothesize(null = 'independence') %>% 
  assume(distribution = 'chisq')

dist

dist %>% 
  visualize()

# test statistic
chisq_stat <- 
  emails %>%
  specify(spam ~ number, success = '1') %>% 
  hypothesize(null = 'independence') %>% 
  calculate(stat = 'chisq')


# p-value
dist %>% 
  get_p_value(obs_stat = chisq_stat, 
              direction = 'right')

# shade p-value on the sampling distribution
dist %>% 
  visualize() +
  shade_p_value(chisq_stat, direction = "right")



# ======================== WINNER vs NUMBER ===================================
# null hypothesis and null distribution
dist <- 
  emails %>% 
  specify(winner ~ number, success = 'yes') %>% 
  hypothesize(null = 'independence') %>% 
  assume(distribution = 'chisq')

dist

dist %>% 
  visualize()

# test statistic
chisq_stat <- 
  emails %>%
  specify(winner ~ number, success = 'yes') %>% 
  hypothesize(null = 'independence') %>% 
  calculate(stat = 'chisq')

chisq_stat


# p-value
dist %>% 
  get_p_value(obs_stat = chisq_stat, 
              direction = 'right')

# shade p-value on the sampling distribution
dist %>% 
  visualize() +
  shade_p_value(chisq_stat, direction = "right")


