#Tidy data 2.0
#Day4
#vhuawelo simba
#01 february day
#final chapter

#load library
library(tidyverse)

# load the data from a .RData file
load("SACTNmonthly_v4.0.RData")

#assign a shorter name to the data.
SACTN <- SACTNmonthly_v4.0

# Remove the original name of the data so you can only remane with the data set you gonna use in the environment
rm(SACTNmonthly_v4.0)


# calculate the mean temperature of each group
SACTN_depth_mean <- SACTN %>% 
  group_by(depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            count = n()
  )

# Why does the relationship between depth and temperature look so odd?
ggplot(SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)+
  labs(x="depth", y=(mean_temp)) +#adding labels
  ggtitle("A") #adding title
  
       
  SACTN_30_years <- SACTN %>% 
    group_by(site, src) %>%
    filter(n() > 360)

  SACTN %>% 
    filter(site == "Paternoster" | site == "Oudekraal") %>%
    group_by(site, src) %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              sd_temp = sd(temp, na.rm = TRUE))
  
#creating a set of sites you want to select.
  selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") 
 
  SACTN %>% 
    filter(site %in% selected_sites) %>% #use %in% when filtering a sites that you concatinated into a set.
    group_by(site, src) %>% 
    summarise(mean_temp = mean(temp, na.rm = TRUE), 
              sd_temp = sd(temp, na.rm = TRUE)) #use summarise function to calculate mean and sd. 
  


