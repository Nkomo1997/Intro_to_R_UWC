#Thulisile Nkomo 
#Exercise 2
#05 February 2018


library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(ggplot2)


load("data/SACTNmonthly_v4.0.RData")
SACTN <- SACTNmonthly_v4.0
rm(SACTNmonthly_v4.0)

temp <- SACTN %>%
  filter(src== "KZNSB") %>% 
  separate(col = date, into =c("year", "month", "day" ), sep = "-") %>% 
  group_by(site, year) 
 

ggplot(temp, aes(x = year, y= temp)) + 
  geom_line(aes(group=site), colour="blue2") +
  facet_wrap(~site, nrow =9) +
  labs(x= "year", y="temperature")+
  scale_x_discrete(breaks=c(1980,2000), labels=c("1980","2000")) +
scale_y_continuous(breaks=c(24,22,20), labels=c("24","22","20")) +
  ggtitle("KZNSB:series of anual means")

lam <- read_csv("data/laminaria.csv")

temp_2 <- lam %>%
  filter(region== "FB") %>% 
  group_by(site) 

plot_1 <- ggplot(temp_2, aes(x = blade_length, y= blade_weight, colour=site)) +
  scale_colour_brewer(palette = "Accent") +
  geom_point() +
  geom_line(aes(group=site)) +
  facet_wrap(~site, nrow =3) +
  labs(x= "blade length", y="blade weight") + 
  ggtitle("A crazy graph of some data at False Bay sites")
plot_1

plott_2 <- ggplot(temp_2, aes(x = blade_length, y= blade_weight, colour=site)) + 
  geom_point() +
  geom_line(aes(group=site)) +
  facet_wrap(~site, nrow =3) +
  labs(x= "blade length", y="blade weight") + 
  ggtitle("A crazy graph of some data at False Bay sites")

ggarrange(plot_1, plott_2,
 labels= c (), common, legend = TRUE) +
  ggtitle("A and B")

  





