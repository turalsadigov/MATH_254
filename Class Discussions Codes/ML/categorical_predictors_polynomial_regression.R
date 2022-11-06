library(stats2data)
library(tidyverse)
library(tidymodels) # ML package

# data wrangling
df <- 
  county %>% 
  select(poverty, homeownership, state) %>%
  filter(state %in% c('New York', 'New Jersey', 'Indiana')) %>% 
  mutate(state = factor(state)) %>% 
  mutate(state = relevel(state, ref = 'Indiana'))
  
# summary
df %>% summary()


# no subgroup
df %>% 
  ggplot(aes(x = homeownership, y = poverty)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)


# use subgroups

df %>% 
  ggplot(aes(x = homeownership, y = poverty, color = state)) +
  geom_point()

# ML

# model specifications
model_1_specs <-
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

model_1_specs  

# fitting a model
model_1_fit <- 
  model_1_specs %>% 
  fit(poverty ~ homeownership + state, data = df)

model_1_fit

# tidy output
tidy(model_1_fit)


# lets plot three lines imposed on the scatterplot

df %>% 
  ggplot(aes(x = homeownership, y = poverty, color = state)) +
  geom_point() +
  geom_abline(slope = -0.248, intercept = 32.2, color = 'red') +
  geom_abline(slope = -0.248, intercept = 32.2 - 0.9307, color = 'blue') +
  geom_abline(slope = -0.248, intercept = 32.2 - 4.20, color = 'green') 


# extract measures pmanually/painfully
my_metrics <- 
  df %>% 
  bind_cols(predict(model_1_fit, new_data = df)) %>% 
  relocate(poverty, .before = .pred) %>% 
  mutate(errors = poverty - .pred, errors_sq = errors^2) %>% 
  mutate(null_preds = mean(poverty), null_errors = poverty - null_preds) %>% 
  summarise(n = dim(df)[1], 
            p = dim(df)[2], 
            TSS = sum(null_errors^2), 
            RSS = sum(errors_sq), 
            unexpl_variance = RSS/TSS, 
            R_sq = 1 - unexpl_variance, 
            R_sq_adj = R_sq - (1-R_sq)*(p/(n-p-1)),
            r = cor(poverty, .pred), 
            r_sq = r^2, 
            MSE = RSS/n, 
            RMSE = sqrt(MSE), 
            RSE = sqrt(RSS/(n-p-1)), 
            ratio = RSE/mean(poverty)) %>% 
  pivot_longer(cols = everything(), 
               names_to = 'metrics', 
               values_to = 'stats')
  
my_metrics


# use in-house commands to extract some measures
model_1_fit %>% 
  glance()


# residual analysis

library(ggfortify)
autoplot(model_1_fit, c(1, 2))


# fit a polynomial model
model_poly_fit <- 
  model_1_specs %>% 
  fit(poverty ~ homeownership + I(homeownership^2) + state ,data = df)

tidy(model_poly_fit)
glance(model_poly_fit)
autoplot(model_poly_fit, c(1, 2))







