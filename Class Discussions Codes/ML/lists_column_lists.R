# data structures in R

x <- c(1, 2, 3)
x
class(x)
typeof(x)

y <- c('a', 'b', 'c')
y
class(y)
y[2]
y[2:3]

z <- c(1 , 2, 'a')
z
class(z)

w <- c(2, TRUE, 3)
w


w <- c(TRUE, FALSE, TRUE)
w
class(w)


s <- c(1, 'a', TRUE)
s
class(s)



## lists

x <- list(1, 2, 3, 'a', TRUE, 0.7)
x
x[[1]]

library(tidyverse)
x <- list(c(1,2,3), 
          lm(mpg~disp, data = mtcars), 
          tibble(a = c(1,2), 
                 b = c(TRUE, FALSE)))
x
x[[1]]
summary(x[[2]])


## column lists

library(stats2data)
df <- county %>% 
  select(state, homeownership, poverty) %>% 
  group_by(state) %>% 
  nest() %>% 
  ungroup()
df
df$data
df$data[[1]]

df

# named lists
x <- list(my_vector = c(1,2,3), 
          my_model = lm(mpg~disp, data = mtcars), 
          my_tibble = tibble(a = c(1,2), 
                             b = c(TRUE, FALSE)))
x
x$my_vector
x$my_model
x$my_tibble
x[[1]]
summary(x[[2]])





