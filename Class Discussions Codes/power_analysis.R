# H0: mu = mu_0
# Ha: mu < mu_0

mu_0 = 131
alpha = 0.05
n = 25
sigma = 10
mu_a = 130

# K = pnorm(qnorm(alpha) + sqrt(n)*(mu_0 - mu_a)/sigma)
# K

my_power <- function(mu_a, mu_0 = 131, n = 25, alpha = 0.05, sigma = 10){
  K = pnorm(qnorm(alpha) + sqrt(n)*(mu_0 - mu_a)/sigma)
  return(K)
}

my_power(mu_a, mu_0, n, alpha, sigma)
my_power(mu_a = 128)

my_power(mu_a = c(128, 129, 130, 131))
my_power(mu_a = seq(120, 131, by = 0.01))

mu_a_s = seq(120, 131, by = 0.01)
powers = my_power(mu_a_s)
plot(mu_a_s, powers, type = 'l', lwd = 3)


# sample size calculations


sample_size_calc <- function(mu_a, K_0, mu_0 = 131, alpha = 0.05, sigma = 10){
  n = (sigma*(qnorm(K_0) - qnorm(alpha))/(mu_0 - mu_a))^2
  return(n)
}


sample_size_calc(mu_a = 128, K_0 = 0.95, alpha = 0.01)

my_power(mu_a = 128, n = 176, alpha = 0.01)


K_0_s = seq(0.80, 0.99, by = 0.001) 
sizes = sample_size_calc(mu_a = 128, K_0 = K_0_s, alpha = 0.01)
plot(sizes, K_0_s, type = 'l',  lwd = 3)













