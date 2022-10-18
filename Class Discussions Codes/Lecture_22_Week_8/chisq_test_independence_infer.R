library(tidyverse)
library(infer)

gun_shift = c(rep('day', 266), 
              rep('even', 585), 
              rep('mid', 734))

df_gun <- tibble(weapon = 'gun', 
                 shift = gun_shift)

knife_shift = c(rep('day', 188), 
                rep('even', 306), 
                rep('mid', 330))
df_knife <- tibble(weapon = 'knife', 
                   shift = knife_shift)


df <- 
  df_gun %>% 
  bind_rows(df_knife)


View(df)

df %>% 
  sample_n(10)

obs_stat <- 
  df %>% 
  specify(weapon~shift) %>% 
  calculate(stat = 'Chisq')

degrees_freedom = (2-1)*(3-1)
degrees_freedom

null_dist <- 
  df %>% 
  specify(weapon~shift) %>% 
  hypothesise(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>% 
  calculate(stat = 'chisq')

null_dist %>% 
  visualise() +
  shade_p_value(obs_stat = obs_stat, 
                direction = 'right')




