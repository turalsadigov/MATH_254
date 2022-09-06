library(tidyverse)
library(infer)

county <- read_csv(file = "https://raw.githubusercontent.com/turalsadigov/MATH_254/main/data/county.csv")

set.seed(2023)
df <- county %>% 
  sample_n(size = 500)
df

set.seed(1267)
df %>% 
  drop_na() %>% 
  rep_sample_n(size = 500,
               replace = TRUE,
               reps = 100) %>% 
  group_by(replicate) %>% 
  summarise(mean = mean(poverty)) %>% 
  ggplot(aes(mean)) +
  geom_density(fill = 'blue', alpha = 0.5)
        
