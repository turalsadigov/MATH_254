# Create Bootstrap distribution of 
# sample median

original_sample = c(2, 3, 5, 7)
B = 1000 # the number of bootstrap samples
sample_meds = rep(0, B) # create placeholder vector of 0's

for (i in 1:B) {
  # create a bootstrap sample
  bootstrap_sample = sample(x = original_sample, 
                            size = 4, replace = T)
  # calculate and store sample statistics (median)
  sample_meds[i] = median(bootstrap_sample)
}

# create bootstrap distribution (approx. sampling distribution)
hist(x = sample_meds, 
     freq = F, 
     col = sample(1:600, 1))
lines(density(sample_meds), 
      lwd = 10)

# get standard error
sd(sample_meds)

# 95% CI for true population median
quantile(x = sample_meds, 
         probs = c(0.025, 0.975))

