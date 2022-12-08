library(tidyverse)
library(stats2data)
library(tidymodels)
library(patchwork)
library(tictoc)
library(beepr)

tic()
ml_steps()
toc()
beep(sound = 2)

df <- 
  county %>% 
  select(pop2017, median_hh_income, metro) %>% 
  mutate(metro = factor(metro)) %>% 
  drop_na()

summary(df)
GGally::ggpairs(df)


## DONT DO THIS AT HOME
bad_model <- 
  nearest_neighbor(neighbors = 50) %>% 
  set_engine('kknn') %>% 
  set_mode('classification') %>% 
  fit(metro ~ ., data = df)

predict(bad_model, new_data = df) %>% 
  bind_cols(df %>% select(metro)) %>% 
  conf_mat(metro, .pred_class)


predict(bad_model, new_data = df) %>% 
  bind_cols(df %>% select(metro)) %>% 
  accuracy(metro, .pred_class)



bad_model %>% 
  augment(df) %>% 
  accuracy(metro, .pred_class)

predict(bad_model, new_data = df_testing) %>% 
  bind_cols(df_testing)



### split data
set.seed(2020)
df_split <- initial_split(df, .80, metro)
df_training <- training(df_split)
df_testing <- testing(df_split)
summary(df_training)
summary(df_testing)


# step1: fit training
model_2 <- 
  nearest_neighbor(neighbors = 50) %>% 
  set_mode('classification') %>% 
  fit(metro ~., data = df_training)

# step2: predict testing

model_2 %>% 
  augment(df_testing) %>% 
  accuracy(metro, .pred_class)

model_2 %>% 
  augment(df_training) %>% 
  accuracy(metro, .pred_class)

# single step


model_2_single <- 
  nearest_neighbor(neighbors = 50) %>% 
  set_mode('classification') %>% 
  last_fit(metro~., df_split)

collect_predictions(model_2_single) %>% 
  accuracy(metro, .pred_class)

## k = 50? 
## k = 10, 50, 200

set.seed(2022)
df_boot_resamples <- 
  bootstraps(df_training, 
             times = 20, 
             strata = metro)


df_boot_resamples %>% 
  unnest(cols = c(splits))


training(df_boot_resamples$splits[[1]])
testing(df_boot_resamples$splits[[1]])



all_cores <- parallel::detectCores(logical = FALSE)
all_cores

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)



tic()
model_10 <- 
  nearest_neighbor(neighbors = 10) %>% 
  set_mode('classification') %>% 
  fit_resamples(metro ~., df_boot_resamples)
toc()
beep(sound = 1)

tic()
model_50 <- 
  nearest_neighbor(neighbors = 50) %>% 
  set_mode('classification') %>% 
  fit_resamples(metro ~., df_boot_resamples)
toc()
beep(sound = 1)

tic()
model_200 <- 
  nearest_neighbor(neighbors = 200) %>% 
  set_mode('classification') %>% 
  fit_resamples(metro ~., df_boot_resamples)
toc()
beep(sound = 1)



collect_metrics(model_10)
collect_metrics(model_50)
collect_metrics(model_200)



## chosen one

save(model_2, file = 'my_amazing_model.RDATA')



# ====== 

# with NAs
df <- 
  county %>% 
  select(pop2017, median_hh_income, metro) %>% 
  mutate(metro = factor(metro))

### split data
set.seed(2020)
df_split <- initial_split(df, .80, metro)
df_training <- training(df_split)
df_testing <- testing(df_split)
summary(df_training)
summary(df_testing)

model_spec <- 
  nearest_neighbor(neighbors = 50) %>% 
  set_mode('classification')

my_rec <- 
  recipe(metro ~., data = df_training) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal()) %>% 
  step_log(pop2017) %>% 
  step_normalize(all_numeric())
  

model_3 <- 
  workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(my_rec) %>% 
  fit(df_training)

model_3












