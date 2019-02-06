#Thulisile Nkomo
#Exercise 2 
#05 February 2019


library(tidyverse)
library(ggpubr)
library(lubridate)
library(dplyr)
library(ggplot2)


load("data/SACTNmonthly_v4.0.RData") #loading data into global environment
SACTN <- SACTNmonthly_v4.0 #changing data name to SACTN to make it easy type and access
rm(SACTNmonthly_v4.0)

temp <- SACTN %>% #changing name from SACTN to temp
  filter(src== "KZNSB") %>% #filtering out the site KZN
  separate(col = date, into =c("year", "month", "day" ), sep = "-") %>% 
  group_by(site, year) #seperating column date into year, month and day. Group it by the site and year

#Creating a graph plot
ggplot(temp, aes(x = year, y= temp)) + 
  geom_line(aes(group=site), colour="blue2") +
  facet_wrap(~site, nrow =9) + #create 9 rows for the sites
  labs(x= "year", y="temperature")+ # creating labels
  scale_x_discrete(breaks=c(1980,2000), labels=c("1980","2000")) + # creating values/scales for the x axis
  scale_y_continuous(breaks=c(24,22,20), labels=c("24","22","20")) + # creating values/scale for the y axis
  ggtitle("KZNSB:series of anual means") 

#PART 2

lam <- read_csv("data/laminaria.csv") #reading the dataset laminaria and remaining it 'lam'

temp_2 <- lam %>% #renaming lam dataset temp_2
  filter(region== "FB") %>%  #filtering out the region False Bay from the laminaria dataset
  group_by(site) #grouping the data by site

plot_1 <- ggplot(temp_2, aes(x = blade_length, y= blade_weight, colour=site)) + # creating a graph
  scale_colour_brewer(palette = "Accent") + 
  geom_point() +
  geom_line(aes(group=site)) +
  facet_wrap(~site, nrow =3) + # creating a new graph with three rows
  labs(x= "blade length", y="blade weight") + 
  ggtitle("A crazy graph of some data at False Bay sites")

plot_2 <- ggplot(temp_2, aes(x = blade_length, y= blade_weight, colour=site)) + 
  geom_point() +
  geom_line(aes(group=site)) +
  facet_wrap(~site, nrow =3) +
  labs(x= "blade length", y="blade weight") + 
  ggtitle("A crazy graph of some data at False Bay sites")


ggarrange(plot_1, plot_1) # combing the two graphs together into a single plot
          
 #PART 4

datasets::ToothGrowth 

toothgrowth_1 <-ToothGrowth %>% #assigning a new name to the dataset 
  group_by(supp, dose) %>% 
  summarise(mean_len = mean(len, na.rm = TRUE), 
            sd_len = sd(len, na.rm = TRUE))
   
toothgrowth <- ggplot(data = toothgrowth_1, aes(x = dose, y = mean_len, fill = supp)) +
  geom_col(aes(fill=supp), position= "dodge", colour= "black") +
  geom_errorbar(aes(ymin= mean_len - sd_len,
                   ymax = mean_len + sd_len), 
                position= "dodge")+
  labs(x= "Dose", y="Tooth length")
  