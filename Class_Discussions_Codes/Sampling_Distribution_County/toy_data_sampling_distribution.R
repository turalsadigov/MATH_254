set.seed(1234)
toy_data = round(runif(n = 7, min = 10, max = 99))
toy_data

choose(7, 3)

toy_data = as_tibble(toy_data)
toy_data

set.seed(2345)
toy_data %>% 
  rep_sample_n(size = 3, reps = 35, replace = F) %>% 
  group_by(replicate) %>% 
  summarise(mean = mean(value)) %>% 
  ggplot(aes(mean)) +
  geom_dotplot()
  
