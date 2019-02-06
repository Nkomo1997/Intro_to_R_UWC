library(tidyverse)
chicks <- datasets::ChickWeight
view(chicks)
??ChickWeight
datasets::airquality

ggplot(data = chicks, aes(x = Time, y = weight)) +
  geom_point() + geom_line(aes(group = Chick))
#aes is aesthetics. first specify what you want to be in your graph
ggplot(data = chicks, aes(x = Time, y = weight, colour =Diet)) + 
  geom_point() + geom_line(aes(group = Chick))

ggplot(data = chicks, aes(x = Time, y = weight)) +
  geom_point() + geom_line(aes(group = Chick))

ggplot(data = chicks, aes(x = Time, y = weight)) +
  geom_point() + geom_smooth(method = "lm")

ggplot(data = chicks, aes(x = Time, y = weight, colour, Diet)) +
  geom_point(color = "blue") + geom_line(aes(group = Chick))

ggplot(data = chicks, aes(x = Time, y = weight, colour = Diet)) + 
  geom_point(aes(size = weight)) + geom_smooth(method = "lm")

ggplot(data = chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + geom_smooth(method = "lm") +
  labs(x = "Days", y = "Weight(kg)") + ggtitle("A") + theme_bw()

# Facetting in ggplot
library(ggpubr)

ggplot(chicks, aes(x = Time, y= weight,colour = Diet)) + 
  geom_point() + geom_smooth(method ="lm") +
  facet_wrap(~Diet, ncol =2)
#ncol is the number of columns

chicks_2 <- chicks %>%
  filter(Time == 21)

plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour =Diet))+ 
  geom_point() + 
  geom_line(aes(group = Chick)) + labs(x ="Days", y = "weight") + 
  ggtitle("A")

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour =Diet)) + 
  geom_point() + geom_smooth(method = "lm") + ggtitle("B")

plot_3 <- ggplot(data = chicks, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100 ) + 
  labs(x = "Final Mass (g)", y = "Count")

plot_4 <- ggplot(data = chicks, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet))+
  labs(x = "Diet", y= "Final Mass (g)")

plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4)


## 3rd library
library(boot)
urine <- boot::urine
#to remove a column this is what we use
urine %>%
  select(-cond)

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))

# [A.A]
# Script not neat
# Script does not run complete thrroughout the document
# Does not gice any introduction in first few lines before doing code
