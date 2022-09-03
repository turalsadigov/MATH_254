# load R data
load("county.rda")

# view data farme
View(county)

# view only few rows
head(county)


# pull out variables/columns
county$pop2000
county$poverty
county$smoking_ban

# or load all columns as their won variables
attach(county)

# summary in categorical variable
table(county$smoking_ban)
table(state)
table(metro)

# dotplot - hsape one varoiable
stripchart(poverty)
stripchart(poverty,  method = 'stack')
# introduce the following plot step by step
stripchart(poverty,  method = 'stack', 
           at = 0.05, pch = 20, xlim = c(0,60),
           col = 'blue', cex = 3, frame.plot = F,
           main = 'Our Fist Dotplot', cex.main = 2)
axis(1, at = seq(0, 60, by = 5))

# shape - histogram - numerical variable
hist(county$poverty, 50)
hist(unemployment_rate)
hist(poverty, 50, freq = F)
hist(unemployment_rate, 50)

# HISTOGRAMS together
hist(poverty, 50, freq = F, ylim = c(0, 0.4), col = 'blue')
hist(unemployment_rate, 50,  freq = F, add = T, col = 'lightgreen')
box()


# boxplot - shape one variable
boxplot(poverty, horizontal = T)
boxplot(poverty, unemployment_rate)



# mean and median
mean(unemployment_rate, na.rm = T)
mean(poverty, na.rm = T)
median(unemployment_rate, na.rm = T)
median(poverty, na.rm = T)

# numerical summaries
summary(poverty)

# spread - IQR, sd, var
var(poverty)
var(poverty, na.rm = T)

sd(poverty)
sd(poverty, na.rm = T)

IQR(poverty)
IQR(poverty, na.rm = T)


# scatterpot - understanding two variables
plot(unemployment_rate, poverty, 
     main = 'Poverty rate vs Unemployment rate in each county',
     xlab = 'Unemployment Rate',
     ylab = 'Poverty Rate', col = 'red')
grid(lwd = 3, col = 'blue')


# reading from CSV
county2 = read.csv('county.csv')






