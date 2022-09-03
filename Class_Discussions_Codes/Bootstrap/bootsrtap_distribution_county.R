set.seed(2023)
df <- county %>% 
  sample_n(size = 500)

set.seed(1267)
df %>% 
  drop_na() %>% 
  rep_sample_n(size = 500,
               replace = T,
               reps = 100) %>% 
  group_by(replicate) %>% 
  summarise(mean = mean(poverty)) %>% 
  ggplot(aes(mean)) +
  geom_density(fill = 'blue', alpha = 0.5)
        
