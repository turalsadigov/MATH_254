# poisson

lambda = 50
n = 10000
set.seed(1)
pois_data <- rpois(n, lambda)
pois_data
mean(pois_data)
var(pois_data)
hist(pois_data, breaks = 10000)
qqnorm(pois_data)
qqline(pois_data)

z <- (pois_data - lambda)/sqrt(lambda)
hist(z, breaks = 10000)
qqnorm(z)
qqline(z)

# normal

n <- 10000
set.seed(21) # anthony from Man United
z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)
z4 <- rnorm(n)
z1
hist(z1, breaks = 100)
hist(z1^2 + z2^2 + z3^2 + z4^2, breaks = 100)

# generate chisq distribution data

y <- rchisq(n = 10000, df = 3)
hist(y, breaks = 100)


# chi squared test: goodness of fit test

# company: ny (50%), nj (25%), ma (25%)
# data: ny (121), nj (83), ma (50)

library(tidyverse)
library(infer)

locs <- c('ny', 'nj', 'ma')
null_prop <- c(.50, .25, .25)
obs_counts <- c(121, 83, 50)

df <- tibble(locs, null_prop, obs_counts)
df

total_customer <- sum(obs_counts)
total_customer


df %>% 
  mutate(exp_counts = total_customer * null_prop) %>% 
  mutate(residuals = (obs_counts - exp_counts)/sqrt(exp_counts)) %>% 
  mutate(z_squared = residuals^2) %>% 
  summarise(sum(z_squared))


pchisq(q = 9.14, df = 2, lower.tail = F)

























