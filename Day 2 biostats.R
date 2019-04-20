#Day2 Bisostats
#Thulisile Nkomo
#Plotting and interpreting graphs

library(fitdistrplus)
library(logspli)

# Generate log-normal data
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)


## summary statistics
## ------
## min:  16.62   max:  54.15 
## median:  40.38 
## mean:  40.28434 
## estimated sd:  7.420034 
## estimated skewness:  -0.551717 
## estimated kurtosis:  3.565162

# normally distributed data
y <- rnorm(100, 13, 2)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)
## summary statistics
## ------
## min:  8.343646   max:  17.38502 
## median:  13.13619 
## mean:  13.01603 
## estimated sd:  1.948029 
## estimated skewness:  -0.02721658 
## estimated kurtosis:  2.591401

# normally distributed data
y <- rnorm(100, 13, 2)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)
## summary statistics
## ------
## min:  8.343646   max:  17.38502 
## median:  13.13619 
## mean:  13.01603 
## estimated sd:  1.948029 
## estimated skewness:  -0.02721658 
## estimated kurtosis:  2.591401

# uniformly distributed data
y <- rexp(100, 0.7)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)
## summary statistics
## ------
## min:  0.0008187769   max:  5.588636 
## median:  0.9319224 
## mean:  1.287164 
## estimated sd:  1.330515 
## estimated skewness:  1.665225 
## estimated kurtosis:  5.214945

y <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y, main = "Histogram of observed data")
plot(density(y), main = "Density estimate of data")
plot(ecdf(y), main = "Empirical cumulative distribution function")
# standardise the data
z.norm <- (y - mean(y)) / sd(y) 
# make a qqplot
qqnorm(z.norm)
# add a 45-degree reference line
abline(0, 1)

#Part 2 of day 2

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000))) #creates a long data frame of a 100 samples 
tail(r_dat)
# Create histogram
library(tidyverse)
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

shapiro.test(r_dat$dat) 

r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat)) # dont do a t-test where the variances are too different. when the difference is 4x then we should be worry about, if it two times then we should not worry about it

# create a single sample of random normal data
set.seed(666) #create a data
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A") # test whether the data is normal or not, use shapeiro test

shapiro.test(r_one$dat) #p value more than . so it is normaly distributed, we can perform a t-test

t.test(r_one$dat, mu = 24) 

ggplot(data = r_one, aes(y = dat, x = sample)) + #BOXPLOT
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()
# check against the trailing tail
t.test(r_one$dat, mu = 30, alternative = "less") #is our daat less then 30

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
 
t.test(dat ~ sample, data = r_two, var.equal = TRUE) 

#Part 3 of day 2

chicks <- datasets::ChickWeight

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

t.test(weight ~ Diet, data = chicks_sub, method = "t.test")

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1) #analysis of variance

#which of the diet will cause the difference.
#draw a graph that will show which of the diet will show a difference. 

plot_1 <- ggplot(data= chicks.aov1, aes(x= Diet, y= weight)) +
  geom_boxplot(aes(fill=Diet), notch=TRUE) +
  labs(x= "Diet", y="Final mass (g)")

plot(TukeyHSD(chicks.aov1))#need to interpret tabel

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))

TukeyHSD(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))

