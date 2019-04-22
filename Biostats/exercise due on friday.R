#Thulisile Nkomo
#Exercises
#Due date, 19 April 2019

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(Rmisc)
library(corrplot)


#Exercise 3.6
# Random normal data
#Null hypothesis= The average amount of time boys and girls ages 7 through 11 spend playing sports each day is believed to be the same.
#Alternative hypthesis= The average amount of time boys and girls ages 7 through 11 spend playing sports each day is believed to not be the same.

r_count <- data.frame(Count = c(rnorm(n = 9, mean = 2, sd = 0.7),
                            rnorm(n = 16, mean = 3, sd = 1)),
                    sample = c(rep("Girls", 9), rep("Boys", 16))) #creating new dataset
shapiro.test(r_dat$dat) #Data is normally distributed

#Create histogram
h <- ggplot(data = r_count, aes(x = Count, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")


t.test(dat ~ sample, data = r_dat, var.equal = TRUE)

#data has a small p.vale less than 0.05, this suggests strong evidence againts null hypothesis therefore it is rejected. We accept the alternative hypothesis

#Exercise 6.7.2
#H0= There is a higher probability of finding girl students in class than finding boys students
#H1= there is a higher probability of finding boy students in class than finding girl students
students <- matrix(c(70, 85, 50, 35), ncol = 2)
colnames(students) <- c("yes", "no")
rownames(students) <- c("girl", "boy")

prop.test(students)

#Conclusion, p is more than 0.05 therefore we accept H0

#Exercise 7.4.1
#Here is bunch of data for pigs raised on different diets. The experiment is similar to the chicken one. Does feed type have an effect on the mass of pigs at the end of the experiment?
# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

#Hypotheis
#H0:The different feed types do not exert influence on the pig mass at the end of the experiment
#H1:The different feed types exert influence on the pig mass at the end of the experiment
#Compairing the mean of mass of different feed types.
mass <- compare_means(mass ~ feed, data = bacon, method = "t.test")
bacon.aov1 <- aov(mass ~ feed, data = bacon)
summary(bacon.aov1)

#conclusion: P value of all mass values were lower than 0.05 which means we reject the null hypothesis
#Thus the different feed types exert influence on the pig mass at the end of the experiment

#Exercise 7.4.2
Tooth<- datasets::ToothGrowth

#Hypothesis
#H0:Both supplements given to the guinea pigs have the same effect on the growth of the teeth of the pigs.
#H1:Both supplements given to the guinea pigs have different effect on the growth of the teeth of the pigs.

Tooth.aov1 <- aov(len ~ supp, data = filter(Tooth, dose %in% c(0.5, 1,2)))
summary(Tooth.aov1 )

#conclusion: P value is higher than 0.05 which means we accept the null hypothesis

#Exercise 7.4.3

beaver <- datasets::beaver1
beaver$activ = as.factor(beaver$activ)
?beaver1

#Hypothesis
#H0: The temperature of the beaver is higher when it is active compared to when it is not being active
#H1: The temperature of the beaver is lower when it is active compared to when it is not being active

beaver.aov1 <- aov(time ~ temp, data = filter(beaver, activ %in% c(0,1)))
summary(beaver.aov1)

#Conclusion, the p value is lower than 0.05 therefore we reject H0 and accept H1

beaver.summary <- beaver%>% 
  group_by(activ) %>% 
  summarise(mean_temp = mean(temp),
            sd_temp = sd(temp)) %>% 
  ungroup()
beaver.summary

beaver.summary2 <- summarySE(data = beaver,
                             measurevar = "temp",
                             groupvars = c("activ"))
#Now we turn to some visual data summaries.

ggplot(data = beaver, aes(x = activ, y = temp)) +
  geom_segment(data = beaver.summary2, aes(x = activ, xend = activ, y = temp - ci, yend = temp + ci, colour = activ),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = activ), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#Exercise 9.6.1

iris <- datasets::iris

cor.test(x = iris$Petal.length, iris$Sepal.length,
         use = "everything", method = "pearson")

iris_sub <- iris %>% 
  dplyr::select(-Species)

iris_pearson <- cor(iris_sub)
iris_pearson

corrplot(iris_pearson, method = "circle")

prop.test(x = 45, n = 100, p = 0.5)
