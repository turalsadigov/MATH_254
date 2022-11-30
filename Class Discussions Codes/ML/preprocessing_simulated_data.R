# simulate data

set.seed(1)
x_1 <- sample(c(rnorm(n = 20, mean = 70, sd = 30), rep(NA, 5)))
x_1
x_2 <- sample(c(rnorm(21, 2, 1), rep(NA, 4)))
x_2
x_3 <- sample(c(rep('A', 13), rep('B', 6), rep(NA, 6)))
x_3
x_4 <- sample(c(runif(5), rep(NA, 20)))
x__5 <- sample(x = c(0,1), size = 25, replace = TRUE)
x...6... <- c(rep(1, 25))
...x....7 <- c(rep(1, 24), 0.99)
x_8 <- sample(c(rep('math', 20), rep('stats', 5)))
`x  9` <- sample(seq(as.Date('2022/01/01'), as.Date('2023/01/01'), by = 'day'), size = 25)
`x  9`
y <- rnorm(25)

df <- tibble(x_1, x_2, x_3, x_4, x__5, x...6..., ...x....7, x_8, `x  9`, y)
df %>% 
  print(n = 25)
