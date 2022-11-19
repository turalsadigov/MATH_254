# here we will fit various methods to a data
# choose one that has better validation error
library(tidymodels)
library(tidyverse)
library(ggfortify)
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
df_split <- ____

df_not_testing <- training(df_split)
df_testing <- testing(df_split)


# split the data into training and validation
set.seed(123)
df_split_2 <- ____

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
  fit(poverty ~ ____, data = ____)

model_2 <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

model_3 <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

model_4 <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

model_5 <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

model_6 <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

# put all objects together
fitted_models <- list(model_1, model_2, model_3,
                      model_4, model_5, model_6 )


# function for validation performance
validation_performance <- function(model){
  model_results <- 
    df_validation %>% 
    bind_cols(predict(____, _____)) %>% 
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
for(model in ____){
  validation_results <- 
    validation_results %>% 
    bind_rows(____(____))
}
validation_results


# CHOOSE A MODEL and do a last fit! 

____ <- 
  model_specs %>% 
  fit(poverty ~ ____, data = ____) 

tidy(____)


# report the following test resulst

test_results <- 
  ____ %>% 
  bind_cols(predict(____, ____)) %>% 
  select(poverty, .pred)

my_metrics <- metric_set(rmse, rsq, mae)

test_results %>% 
  my_metrics(poverty, .pred)

library(vip)
____(____)


# =========== saving the model for future, deploying a model ==============

save(____, file = 'poverty_final_model.Rdata')


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