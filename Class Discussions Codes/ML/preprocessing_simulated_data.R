# simulate data
library(tidyverse)
library(tidymodels)

set.seed(1)
x_1 <- sample(c(rnorm(n = 20, mean = 70, sd = 30), rep(NA, 5)))
x_1
x_2 <- sample(c(rnorm(21, 2, 1), rep(NA, 4)))
x_2
x_3 <- sample(c(rep('A', 13), rep('B', 6), rep(NA, 6)))
x_3
x_4 <- sample(c(runif(5), rep(NA, 20)))
x_4
x__5 <- sample(x = c(0,1), size = 25, replace = TRUE)
x__5
x...6... <- rep(1, 25)
...x....7 <- c(rep(1, 24), 0.99)
x_8 <- sample(c(rep('math', 20), rep('stats', 5)))
`x  9` <- sample(seq(as.Date('2022/01/01'), as.Date('2023/01/01'), by = 'day'), size = 25)
`x  9`
x_10 <- 3*x_1 + rnorm(25)
y <- rnorm(25)

df <- tibble(x_1, x_2, x_3, x_4, x__5, x...6..., ...x....7, x_8, `x  9`, x_10, y)
df %>% 
  print(n = 25)



# drop na
df %>% drop_na()


df2 <- df %>% 
  clean_names() %>% 
  mutate_if(is.character, factor) %>% 
  mutate(x_5 = factor(x_5))
df2  


# split data

set.seed(2022)
df_split <- initial_split(df2, .80, y)

df_tr <- training(df_split)
df_ts <- testing(df_split)


## recipes package: preprocessing 
## and feature engineering

first_rec <- recipe(y ~ ., data = df_tr)
first_rec
summary(first_rec)

second_rec <- 
  recipe(y~., data = df_tr) %>% 
  step_date(x_9, features = c('dow', 'month', 'doy')) %>% 
  step_holiday(x_9) %>% 
  update_role(x_9, new_role = 'id')


second_rec
summary(second_rec)

main_rec <- 
  second_rec %>% 
  themis::step_upsample(x_8, id = 'my_upsample') %>% 
  step_filter_missing(all_predictors(), threshold = .70) %>% 
  step_impute_median(all_numeric()) %>% 
  step_impute_mode(all_nominal()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.85) %>% 
  prep()

main_rec
summary(main_rec) 
  
## modeling


knn_reg_specs <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine('kknn') %>% 
  set_mode('regression')



my_model <- 
  workflow() %>% 
  add_model(knn_reg_specs) %>% 
  add_recipe(main_rec) %>% 
  last_fit(df_split)

my_model %>% 
  collect_metrics()

my_model %>% 
  collect_predictions()











