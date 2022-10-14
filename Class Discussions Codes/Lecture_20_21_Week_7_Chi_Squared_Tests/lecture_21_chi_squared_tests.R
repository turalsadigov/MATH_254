library(tidyverse)
library(infer)

book <- c('buy', 'print', 'oline')
null_props <- c(0.10, 0.20, 0.70)
obs_counts <- c(0, 0, 20)

df <- tibble(book, null_props, obs_counts)
df

df %>% 
  mutate(exp_counts = 20*null_props) %>% 
  mutate(residuals = (obs_counts - exp_counts)/sqrt(exp_counts)) %>% 
  mutate(residuals_sq = residuals^2) %>% 
  summarise(chi_squared = sum(residuals_sq))


p_value = pchisq(q = 8.57, df = 2, lower.tail = F)
p_value

### BASE R way of conducting the test

tst <- chisq.test(x = c(1, 1, 18), p = c(0.10, 0.20, 0.70))
tst$statistic
tst$parameter
tst$p.value
tst$method
tst$observed
tst$expected
tst$residuals


### infer way of conducting the test

students <- c(rep('buy', 1), rep('print', 1), rep('online', 18))
students <- factor(students, levels = c('buy', 'print', 'online'))

df <- tibble(students)
df

obs_stat <- 
  df %>% 
  specify(response = students) %>% 
  hypothesise(null = 'point', p = c('buy' = .10,
                                    'print' = .20,
                                    'online' = .70)) %>% 
  calculate(stat = "Chisq")

obs_stat

null_dist <- 
  df %>% 
  specify(response = students) %>% 
  hypothesise(null = 'point', p = c('buy' = .10,
                                    'print' = .20,
                                    'online' = .70)) %>%
  generate(reps = 1000, type = 'draw') %>% 
  calculate(stat = 'Chisq')

null_dist %>% 
  visualise()


null_dist %>% 
  get_p_value(obs_stat, direction = 'right')


#### chi squared test of independence


r <- c(770, 230)
d <- c(60, 1140)
m <- matrix(c(r, d), nrow = 2, ncol = 2)
m
colnames(m) <- c('r', 'd')
rownames(m) <- c('disapprove', 'approve')
m

tst <- chisq.test(m)
tst$statistic
tst$parameter
tst$p.value
tst$observed
tst$expected
tst$residuals


