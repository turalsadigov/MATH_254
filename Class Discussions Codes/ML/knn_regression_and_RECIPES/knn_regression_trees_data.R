
# libraries
library(tidymodels)
library(tidyverse)
library(stats2data)

# scatterplot
stats2data::trees %>% 
  ggplot(aes(x = Diam, y = Vol)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

# transformations
df <- stats2data::trees %>% 
  mutate(log_diam = log(Diam), log_vol = log(Vol))

# transformed scatterplot
df %>% 
  ggplot(aes(x = log_diam, y = log_vol)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

# lin regression model from a past lab
old_model <- 
  linear_reg() %>% 
  fit(log_vol ~ log_diam, df)

tidy(old_model)

# old model Rsq
predict(old_model, df) %>% 
  mutate(old_pred = exp(.pred)) %>% 
  bind_cols(df) %>% 
  yardstick::rsq(Vol, old_pred)
  
# old model predictions
predict(old_model, df) %>% 
  mutate(old_pred = exp(.pred)) %>% 
  bind_cols(df) %>% 
  ggplot(aes(x = Diam)) +
  geom_point(aes(y = Vol)) +
  geom_line(aes(y = old_pred))
  


## KNN regression with 10 neighbors

knn_reg_model <- 
  nearest_neighbor(neighbors = 1) %>% 
  set_engine('kknn') %>% 
  set_mode('regression') %>% 
  fit(Vol ~ Diam, data = df)

knn_reg_model 

## knn reg predictions
predict(knn_reg_model, df) %>% 
  bind_cols(df) %>% 
  ggplot(aes(x = Diam)) +
  geom_point(aes(y = Vol)) +
  geom_step(aes(y = .pred))
  

# ================== combine everything and plot

df %>% 
  bind_cols(predict(old_model, df)) %>% 
  mutate(old_pred = exp(.pred)) %>% 
  select(Diam, Vol, old_pred) %>% 
  bind_cols(predict(knn_reg_model, df)) %>% 
  rename(knn_pred = .pred) %>% 
  ggplot(aes(x = Diam)) +
  geom_point(aes(y = Vol)) +
  geom_line(aes(y = old_pred), color = 'darkgreen') +
  geom_step(aes(y = knn_pred), color = 'blue')




  
  


