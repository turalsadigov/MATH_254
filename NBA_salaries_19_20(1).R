# NBA salaries for 2019-20

# pull up the data - DOWNLOAD DATASET FROM BLACKBOARD AND COPY PASTE PATHNAME BELOW

nba = read.table(file = 'nba_salaries_text',
                   header = T)
View(nba_salaries)
nba_salaries = nba$Salary
nba_salaries


# population distribution - ALL NBA players' salaries 2019-20
par(mfrow = c(1,1))
hist(nba_salaries,20, col = 'light blue', 
     freq = F,
     main = 'Distribution of NBA salaries - 2019/20 season',
     xlab = 'NBA salaries (in million $)')
axis(1, at = seq(0, 40, by = 5))

lines(density(nba_salaries), lwd = 3, col = 'red')

summary(nba_salaries)
sd(nba_salaries)


# sample mean of 10 players' salaries
# is it a good estimate for true population mean = $ 7.708 millions

# how many samples are possible?
length(nba_salaries)
choose(488, 10)

# DO NOT RUN THE FOLLOWING CODE
# all means from sample of 10
# combn(x = nba_salaries, m = 10, FUN = mean) - too many

# lets do random samples instead

m = 200 # number of simulations
salary_means = rep(0, m) # repeat many 0's

# sample size of 10
for(i in 1:m){
  sample_ten = sample(x = nba_salaries, size = 10, replace = FALSE)
  salary_means[i] = mean(sample_ten)
}

# histogram
hist(x = salary_means, 20, xlim = c(0,40),
     main = 'Simulated sampling distribution of means of salaries of sample of 10 workers',
     col = 'light green',
     xlab = 'Sample means of salaries')

mean(salary_means) # vs mean(nba_salaries)
mean(nba_salaries)

sd(nba_salaries)/sqrt(10) # vs sd(nba_salaries)
sd(salary_means)


# how close will an estimate be?

L = mean(salary_means) - 2*sd(salary_means) 
R = mean(salary_means) + 2*sd(salary_means)

cat('(', L, R, ')')

2*sd(salary_means)

# do the experiment once
sample_ten = sample(x = nba_salaries, 
                    size = 10, 
                    replace = FALSE)
mean(sample_ten)

# ============== Sampling distribution for various sample sizes =======

par(mfrow = c(2,3))
m = 200 # number of samples
n = c(1, 5, 10, 20, 50) # sample sizes
s = c()

for(j in 1:5){
  salary_means = rep(0, m)
  for(i in 1:m){
    sample_n = sample(x = nba_salaries, size = n[j], replace = FALSE)
    salary_means[i] = mean(sample_n)
  }
  hist(x = salary_means, 10, xlim = c(0,40), col = sample(1:600, size = 1),
       main = paste("Sample size = ", n[j]))
  s = c(s, sd(salary_means))
}
plot(x = n, y = s, type = 'l',
     xlab = 'Sample size', ylab = 'SE of sample mean', 
     main = 'SE of sample mean vs sample size',
     pch = 20, col = 'blue', cex = 2)
points(x = n, y = s, pch = 20, col = 'red', cex = 2)

# ============ Calculating probabilities ======================

# prob X^bar_40 <= 3

pnorm(q = 3, mean = mean(nba_salaries), sd = sd(nba_salaries)/sqrt(40))

mean(nba_salaries)- 2*sd(nba_salaries)/sqrt(40)
mean(nba_salaries) + 2*sd(nba_salaries)/sqrt(40)

pnorm(q = 8, mean = mean(nba_salaries), sd = sd(nba_salaries)/sqrt(40), lower.tail = F)

#===================== HOW ABOUT ESTIMATING POPULATION MAXIMUM? ========

# lets do random samples instead

n = 500
sample_maxs = rep(0, n)
for(i in 1:n){
  sample_ten = sample(x = nba_salaries, size = 10, replace = FALSE)
  sample_maxs[i] = max(sample_ten)
}

# histogram
par(mfrow = c(1,1))
hist(x = sample_maxs, 20,
     main = 'Simulated sampling distribution of maximums of salaries of sample of 10 workers',
     freq = F,
     col = 'grey',
     xlab = 'Sample maximums')
lines(density(sample_maxs), lwd = 4, col = 'red')

mean(sample_maxs) # vs mean(nba_salaries)
sd(sample_maxs) # vs sd(nba_salaries)

