# creating a confidence interval
p = 0.88
n = 1000
set.seed(1000)
mysample <-  rbinom(n = n, size = 1, prob = p)
phat  <-  mean(mysample)
l <-  phat - 1.96*sqrt(phat*(1-phat)/n)
r <-  phat + 1.96*sqrt(phat*(1-phat)/n)
cat(l, r)

# OR
prop.test(x = phat*1000, 
          n = n, 
          conf.level = .95)

# CREATING 20 CONFIDENCE INTERVALS
par(mfrow = c(1,1))
p <-  0.88
n <-  1000
x <-  rep(p, 21)
y <-  seq(0, 20)
plot(x,y, 'l', 
     xlim = c(0.8, 0.95), 
     col = 'red', 
     xlab = 'Sample proportion',
     ylab = 'Sample index')
grid(nx = 30, ny = 22)
points(0.88, 0, col = 'red', cex = 2, pch = 20)
abline(0, 0)

set.seed(2021)
for(i in 1:20){
  mysample <-  rbinom(n = n, size = 1, prob = p)
  phat  <-  mean(mysample)
  l <-  phat - 1.96*sqrt(phat*(1-phat)/n)
  r <-  phat + 1.96*sqrt(phat*(1-phat)/n)
  Sys.sleep(1)
  lines(seq(l, r, length.out = 10), rep(i, 10), type = 'l', col = 'blue')
  points(l, i, pch = 20, cex = 2, col = 'green')
  points(r, i, pch = 20, cex = 2, col = 'green')
  if(l>p | r<p){
    lines(seq(l, r, length.out = 10), rep(i, 10), type = 'l', col = 'black', lwd = 3)
    points(l, i, pch = 20, cex = 2, col = 'black')
    points(r, i, pch = 20, cex = 2, col = 'black')
  }
  points(phat, i, pch = 4, cex = 1, col = 'red')
}

