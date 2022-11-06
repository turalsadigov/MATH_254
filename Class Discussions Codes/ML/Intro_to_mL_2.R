library(tidymodels)
library(tidyverse)
library(GGally)
library(plotly)
library(ggfortify)


# install.packages(c('GGally', 'plotly', 'ggfortify'))

# data
ad <- read_csv('https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/advertising.csv')
View(ad)


model <- 
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')


model_1_fitted <- 
  model_1 %>% 
  fit(Sales ~ TV, data = ad)

model_1_fitted %>% 
  extract_fit_engine() %>% 
  summary()


model_1_fitted %>% 
  tidy()

model_1_fitted %>% 
  glance()

library(ggfortify)
dev.off()
autoplot(model_1_fitted, c(1,2))


# lets make another model: sales ~ tv + radio

library(plotly)
ad %>% 
  plot_ly(x = ~TV, y = ~Radio, z = ~Sales) %>% 
  add_markers()



model_2_fitted <- 
  model %>% 
  fit(Sales ~ TV + Radio, data = ad)
  
tidy(model_2_fitted)
glance(model_2_fitted)[1:5]

autoplot(model_2_fitted, c(1,2))
  















