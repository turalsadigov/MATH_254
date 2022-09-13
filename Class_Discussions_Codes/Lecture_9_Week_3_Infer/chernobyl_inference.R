library(tidymodels)
library(tidyverse)

n = 0.16*244
feather <- rep('white_feather', n)
no_feather <- rep('no_white_feather', 244-n)
set.seed(2022)
chernobyl <- sample(c(feather, no_feather))
chernobyl

chernobyl_df <- tibble(chernobyl)
chernobyl_df

chernobyl_df %>% 
  count(chernobyl)

p_hat <-   
  chernobyl_df %>% 
  specify(response = chernobyl, 
          success = 'white_feather') %>% 
  calculate(stat = 'prop')
p_hat
# get observed test stats

z_stat<- 
  chernobyl_df %>% 
  specify(response = chernobyl, success = 'white_feather') %>% 
  hypothesize(null = 'point', p = .02) %>% 
  calculate(stat = 'z')
z_stat

# double check!

(p_hat - 0.02)/(sqrt(0.02*0.98/244))

# infer and prop test

set.seed(2022)
z_dist <- 
  chernobyl_df %>% 
  specify(response = chernobyl, success = 'white_feather') %>% 
  hypothesize(null = 'point', p = 0.02) %>% 
  assume(distribution = 'z') 


z_dist %>% 
  get_p_value(obs_stat = z_stat, direction = 'right')

z_dist %>% 
  visualize() +
  shade_p_value(z_stat, direction = "right")

