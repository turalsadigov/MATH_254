library(tidyverse)
library(infer)
county <- read_csv(file = "https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/county.csv", 
              show_col_types = F)
county %>% 
  head()

set.seed(2022)
county %>% 
  drop_na() %>% 
  rep_sample_n(size = 100, 
               replace = F, 
               reps = 500) %>% 
  group_by(replicate) %>% 
  summarise(max = max(poverty), 
            ave = mean(poverty), 
            med = median(poverty), 
            sd = sd(poverty), 
            iqr = IQR(poverty)) %>% 
  summarise(mean_maxs = mean(max), 
            mean_means = mean(ave), 
            mean_meds = mean(med), 
            mean_sds = mean(sd), 
            mean_iqrs = mean(iqr))



set.seed(2022)
sampling_stats <- 
  county %>% 
  drop_na() %>% 
  rep_sample_n(size = 100, 
               replace = F, 
               reps = 500) %>% 
  group_by(replicate) %>% 
  summarise(max = max(poverty), 
            ave = mean(poverty), 
            med = median(poverty), 
            sd = sd(poverty), 
            iqr = IQR(poverty))


sampling_stats %>% 
  ggplot(aes(x = max))+
  geom_density(fill = 'blue', 
               alpha = 0.5)

sampling_stats %>% 
  ggplot(aes(x = ave))+
  geom_density(fill = 'blue', 
               alpha = 0.5)
sampling_stats %>% 
  ggplot(aes(x = med))+
  geom_density(fill = 'blue', 
               alpha = 0.5)
