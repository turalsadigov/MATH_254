# here we will fit various methods to a data
# choose one that has better validation error
library(tidymodels)
library(tidyverse)
library(vip)
library(stats2data)

# pull out three variables
# make Indiana base state variable
df <- county %>% 
  select(state, poverty, homeownership) %>% 
  filter(state %in% c('New York', 'New Jersey', 'Indiana')) %>% 
  mutate(state = factor(state)) %>% 
  mutate(state = relevel(state, ref = 'Indiana'))

# split the data into training&validation and testing 
set.seed(2022)
df_split <- initial_split(data = df, prop = 0.80, strata = poverty)
df_split

df_not_testing <- training(df_split)
df_testing <- testing(df_split)


# split the data into training and validation
set.seed(123)
df_split_2 <- initial_split(data = df_not_testing, prop = .75, strata = poverty)
df_split_2
df_training <- training(df_split_2)
df_validation <- testing(df_split_2)

# model specifications
model_specs <- 
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') 

# fir various models
model_1 <- 
  model_specs %>% 
  fit(poverty ~ homeownership, data = df_training)

model_2 <- 
  model_specs %>% 
  fit(poverty ~ state, data = df_training) 

model_3 <- 
  model_specs %>% 
  fit(poverty ~ homeownership + state, data = df_training) 

model_4 <- 
  model_specs %>% 
  fit(poverty ~ poly(homeownership, 2, raw = TRUE), data = df_training) 

model_5 <- 
  model_specs %>% 
  fit(poverty ~ poly(homeownership, 3, raw = TRUE), data = df_training) 

model_6 <- 
  model_specs %>% 
  fit(poverty ~ homeownership*state, data = df_training) 

# put all objects together
fitted_models <- list(model_1, model_2, model_3,
                      model_4, model_5, model_6 )


# function for validation performance
validation_performance <- function(model){
  model_results <- 
    df_validation %>% 
    bind_cols(predict(model, df_validation)) %>% 
    select(poverty, .pred)
  my_metrics <- metric_set(rmse, rsq, mae)
  output <- 
    model_results %>% 
    my_metrics(poverty, .pred) %>% 
    select(-.estimator) %>% 
    pivot_wider(names_from = .metric, values_from = .estimate)
  return(output)
}


# look at each model
validation_results <- tibble()
for(model in fitted_models){
  validation_results <- 
    validation_results %>% 
    bind_rows(validation_performance(model))
}
validation_results


# CHOOSE A MODEL and do a last fit! 

model_3_updated <- 
  model_specs %>% 
  fit(poverty ~ ., data = df_not_testing) 

tidy(model_3_updated)


# report the following test resulst

test_results <- 
  df_testing %>% 
  bind_cols(predict(model_3_updated, df_testing)) %>% 
  select(poverty, .pred)

test_results
my_metrics <- metric_set(rmse, rsq, mae)

test_results %>% 
  my_metrics(poverty, .pred)

library(vip)
____(____)


# =========== saving the model for future, deploying a model ==============

save(model_3_updated, file = 'poverty_final_model.Rdata')


# version and deploy model
library(vetiver)
v <- 
  ____ %>% 
  extract_fit_engine() %>% 
  vetiver_model(model_name = 'poverty_model')
v
library(pins)
model_board <- board_temp()
model_board %>% 
  vetiver_pin_write(v)

library(plumber)
pr() %>%
  vetiver_api(v) %>%
  pr_run(port = 8088)