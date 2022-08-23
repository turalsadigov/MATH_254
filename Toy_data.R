# Sampling distribution of a toy example

toy_data = c(25, 30, 40, 50, 55, 64)

# sample mean of sample of two
combn(x = toy_data, m = 2)
sample_means_of_two = combn(x = toy_data, 
                            m = 2, 
                            FUN = mean)

# dotplot
stripchart(sample_means_of_two,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of two', cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_two), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)

# sample mean of sample of three
combn(x = toy_data, m = 3)
sample_means_of_three = combn(x = toy_data, 
                              m = 3, 
                              FUN = mean)

# dotplot
stripchart(sample_means_of_three,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of three', cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_three), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)

# sample mean of sample of four
combn(x = toy_data, m = 4)
sample_means_of_four = combn(x = toy_data, 
                             m = 4, 
                             FUN = mean)

# dotplot
stripchart(sample_means_of_four,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of four', cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_four), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)

# =============== all dotplots in one frame ============
par(mfrow = c(3, 1))
stripchart(sample_means_of_two,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of two', 
           cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_two), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)
stripchart(sample_means_of_three,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of three', 
           cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_three), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)
stripchart(sample_means_of_four,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(25,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Distribution of sample mean of four', 
           cex.main = 2)
axis(1, at = seq(25, 60, by = 1))
points(mean(sample_means_of_four), 0,  
       col = 'red', 
       cex = 4,
       pch = 20)