library(tidyverse)
library(infer)

delays <- read_csv('https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/delays.csv')
delays


delays %>% 
  ggplot(aes(`Departure Delay Minutes`)) +
  geom_density(fill = 'blue', alpha = 0.4)


delays %>% 
  count()

set.seed(2023)
delays_sample <- 
  delays %>% 
  slice_sample(n = 100)

delays_sample %>% 
  ggplot(aes(`Departure Delay Minutes`)) +
  geom_density(fill = 'blue', alpha = 0.4)

set.seed(1234)
delays_sample %>% 
  rep_sample_n(size = 100, 
               replace = T, 
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(meds = median(`Departure Delay Minutes`)) %>% 
  ggplot(aes(meds)) +
  geom_density(fill = 'blue', alpha = 0.4)
  
set.seed(1234)
delays_sample %>% 
  rep_sample_n(size = 100, 
               replace = T, 
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(meds = median(`Departure Delay Minutes`)) %>% 
  summarise(ave_medians = mean(meds), 
            left = quantile(meds, 0.025),
            right = quantile(meds, 0.975))



median(delays$`Departure Delay Minutes`)
mean()



set.seed(1234)
meds <- delays_sample %>% 
  rep_sample_n(size = 100, 
               replace = T, 
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(meds = median(`Departure Delay Minutes`)) %>%
  pull(meds)
meds
summary(meds)
quantile(meds, c(0.025, 0.975))
