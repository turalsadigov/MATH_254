library(tidymodels)
library(tidyverse)
library(GGally)
library(plotly)
library(ggfortify)


# install.packages(c('GGally', 'plotly', 'ggfortify'))

# data
ad <- read_csv('https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/advertising.csv')
View(ad)

# plot
plot(ad)

ad %>% 
  ggpairs()


ad %>% 
  ggplot(aes(x = TV, y = Sales)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_hline(yintercept = 15.1305)


# base R

model <- lm(Sales ~ TV, data = ad)
summary(model)

# tidymodels

new_model <- 
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>% 
  fit(Sales ~ TV, data = ad)

new_model %>% 
  tidy()

tidy(new_model)


new_model %>% 
  glance()


# predictions


new_model %>% 
  predict(new_data = ad)


ad %>% 
  dplyr::select(TV, Sales) %>% 
  bind_cols(predict(new_model, new_data = ad)) %>% 
  mutate(ress = Sales - .pred, ress_sq = ress^2) %>% 
  summarise(RSS = sum(ress_sq), 
            TSS = sum((Sales - 15.1305)^2), 
            perc_not_explained = RSS/TSS, 
            R_sq = 1 - perc_not_explained, 
            r = cor(Sales, .pred), 
            r_sq = r^2)




















