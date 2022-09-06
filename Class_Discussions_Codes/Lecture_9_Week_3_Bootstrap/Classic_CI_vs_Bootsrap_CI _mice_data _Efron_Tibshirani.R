# Treatment of mice with new medical treatment
# Efron and Tibshirani - Modern fathers of statistics

x = c(94, 197, 16, 38, 99, 141, 23)
y = c(52, 104, 146, 10, 51, 30, 40, 27, 46)

# ================= Classical method ====================================
# calculate test stats
xbar = mean(x)
s_xbar = sd(x)/sqrt(length(x))

ybar = mean(y)
s_ybar = sd(y)/sqrt(length(y))

xbarybar = mean(x)-mean(y)
s_xbarybar = sqrt((sd(x)/sqrt(length(x)))^2+(sd(y)/sqrt(length(y)))^2)

t = (xbarybar - 0)/s_xbarybar

# t-test
t.test(x = x, y = y, alternative = 'greater', mu = 0, conf.level = .95)
# two-sided 95% CI 
t.test(x = x, y = y, mu = 0, conf.level = .95)

# ================ Bootstrap method ======================================

# SAMPLE MEAN DIFFERENCE
B = 1000
mean_differences = rep(0, B)
for(i in 1:B){
  bootstrap_x = sample(x = x, size = 7, replace = T)
  bootstrap_y = sample(x = y, size = 9, replace = T)
  mean_differences[i] = mean(bootstrap_x) - mean(bootstrap_y)
}
hist(mean_differences)
sd(mean_differences)

qqnorm(mean_differences)
qqline(mean_differences)

quantile(x = mean_differences)
quantile(x = mean_differences, c(0.025, 0.975))


# SAMPLE MEDIAN DIFFERENCE
B = 5000
med_differences = rep(0, B)
for(i in 1:B){
  bootstrap_x = sample(x = x, size = 7, replace = T)
  bootstrap_y = sample(x = y, size = 9, replace = T)
  med_differences[i] = median(bootstrap_x) - median(bootstrap_y)
}
hist(med_differences)
sd(med_differences)

qqnorm(med_differences)
qqline(med_differences)

quantile(x = med_differences)
quantile(x = med_differences, c(0.025, 0.975))

