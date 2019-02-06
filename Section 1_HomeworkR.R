#Thulisile Nkomo
#01 February 2019
#Homework Section 1, 2,3 and 4


#Loading  all libraries that i will make use of their functions.
library(tidyverse)
library(scales)
library(ggsn)
library(ggpubr)
library(lubridate)
library(grDevices)

#Loading data (rast_feb and rast_aug)
#Because both datasets are RData files we will use the function load().

load("data/rast_feb.RData")
load("data/rast_aug.RData")

#Exploring both my datasets individually:
#Summary function to get the overview of the rast_feb and rast_aug dataset.
#Using rast_feb dataset to explore the top half and bottom half of the data.
summary(rast_feb) 
head(rast_feb)  
tail(rast_feb)

summary(rast_aug)
head(rast_aug)
tail(rast_aug)

#Then count the number of rows. Pipe after to add other codes 
#Use the head and tail functions to display only the first 4 top and 5 bottom lines of the dataset.
#Select the temp

nrow(rast_feb) 
head(rast_feb, n =4)  
tail(rast_feb, n =5)

#Display 3 top and 2 bottom list of the data.
nrow(rast_aug)
head(rast_aug,n= 3)
tail(rast_aug, n =2)

#dimensions'
dim(ras_aug)
dim(ras_feb)

#Display only the season and temperature and this assigned a new name
#So you assign a new name first
seas_temp  <- rast_feb %>%
  select(season, temp)

#Assign new name then display all variables except the index variable
aug_new <- rast_aug %>% 
  select(-index)


#Then display the highest and lowest temperature.
max_temp <-  rast_feb %>%
  select(season, temp) %>% 
  filter(temp == max(temp))

#Display the lowest temperature for august
min_temp <- rast_aug %>% 
  select(season, temp) %>% 
  filter(temp ==min(temp)) 

#Using glimpse() and str() to preview our data and see howe console.
glimpse(rast_aug)
str(rast_aug) #new function
dim(rast_feb) #looking at the data dimensions

#Calculations
rast_aug %>% 
  summarise(temp = mean(temp),
            sd_temp =sd(temp),
            var_temp =var(temp))



# Plotting/Putting the points for both datasets using the lon and lat variables.Showing only the 

  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
           geom_point(colour ="grey") 
  
  
  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
      geom_point(colour ="blue2")
  
 
#Creating own colour pallete using the link
  colPal <- c("#75E1FA,#5CB9CC,#4592A0,#316D76,
              #1E4A4F,#0D292C#5389A0,#49748A,
              #3F6174,#344E5F,#293D4B,#1E2C37")

#Listing all colours available so i can choose any i want. 
grDevices::colors() #function not taught in class.

load("data/sa_provinces.RData")


#Making use of the colour pallete I created on both maps. 
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    geom_polygon(colour = "black", fill = "grey70", aes(group = index))+
    geom_path(data = rast_aug, group= index)
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_aug, aes(fill = bins)) +
  scale_fill_manual("Temp. (°C)", values = colPal)
  
  ggplot(data =rast_feb, aes(x = lon,y = lat)) +
    geom_point(colour ="blue2") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_feb, aes(fill = bins)) +
    scale_fill_manual("Temp. (°C)", values = colPal)
  
  #Adding labels and title on both graphs.
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") 
  
  
  ggplot(data =rast_feb, aes(x = lon, y = lat)) +
    geom_point(colour ="blue2") +
    geom_path(data = sa_provinces, aes(group = group))+
  labs(x ="Longtde", y ="Latd") +
    ggtitle("Feb-Map")
 
   
#Adding oceans names (Atlanic and indian ocean) on the map then increase the size of the labels.
  
  ggplot(data =rast_aug, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude", size =) +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_aug, aes(fill = bins)) +
    geom_polygon(colour = "ivory2", fill = "grey75")+
    scale_fill_manual("Temp. (°C)", values = colPal)
  
  #Code to add ocean names
    annotate("text", label = "Atlantic\nOcean",
             x = 15.1, y = -34.0,
             size = 4.0,
             angle = 35,
             colour = "navy") +
    annotate("text", label = "Indian\nOcean",
             x = 33.2, y = -34.2,
             size = 5.0,
             angle = 330,
             colour = "springgreen")
    
    #FEBRUARY
     ggplot(data =rast_feb, aes(x = lon,y = lat)) +
    geom_point(colour ="grey") +
    labs(x ="Longitude", y ="Latitude") +
    ggtitle("Aug_Map") +
    geom_raster(data = rast_feb, aes(fill = bins)) +
    geom_polygon(colour = "turquoise", fill = "grey50")+
    scale_fill_manual("Temp. (°C)", values = colPal) 
    
    
#Scale Bar 
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
    north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
          scale = 1.2, symbol = 16)
  
  
# SECTION 2
  
Eck <- read_csv("data/ecklonia.csv")

#Reviewing dataset of ecklonia
summary(Eck) #Allows an overview of the dataset ecklonia
head(Eck)  #Allows overview of the top section on the dataset
tail(Eck)  #Allows overview of the top section on the dataset

nrow(Eck) #Counting the number of rows 
head(Eck, n =3)  #Dataset will only show the first three colomns
tail(Eck, n =3) #Dataset will only show the last three columns

# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses

plot_1 <- ggplot(Eck, aes(x = stipe_length, y = stipe_diameter, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = site)) + labs(x ="stipe_length", y = "stipe_diameter") + 
  ggtitle("A line graph showing the stipe length and stipe diameter of the species 
          ecklonia at different sites")
plot_2 <- ggplot(data=ecklonia,aes(x= primary_blade_width)) +
  geom_bar(aes(fill= site), position = "dodge") +
  labs(x = "Primary blwid", y= "Count") +
  ggtitle("C")

plot_3 <- ggplot(data = Eck, aes(x = stipe_length, y = stipe_diameter)) +
  geom_boxplot(aes(fill = site))+
  labs(x = "stipe length", y= "stipe diameter")

# Make use of the ggarrange function and arrange these three graphs created above into 1 plot

ggarrange(plot_1, plot_2, plotZ_3,
          ncol = 2, nrow = 2,
          common.legend = TRUE)

# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites

Eck %>%  #select ecklonia dataset and calculate for stipe length
  group_by(site) %>%
  summarise(avrg_sl = mean(stipe_length),
            med_sl = median(stipe_length),
            stdev_sl = sd(stipe_length),
             var_sl = var(stipe_length),
            med_sl = median(stipe_length),
            min_sl = min(stipe_length),
            max_sl = max(stipe_length)) #The piped dataset, then summarise, and give it a new name could have used another term instead of e.g avrg_bl

Eck %>% #select ecklonia dataset and calculate for stipe diameter
  group_by(site) %>%
  summarise(avrg_sd = mean(stipe_diameter),
            med_sd = median(stipe_diameter),
            stdev_sd = sd(stipe_diameter),
            var_sd = var(stipe_diameter),
            med_sd = median(stipe_diameter),
            min_sd = min(stipe_diameter),
            max_sd = max(stipe_diameter))

# Calculate standard error

Eck %>% 
  group_by(site) %>% #group my data by site
  summarise(var_bl = var(stipe_length), #then summarise for stipe length
            n = n()) %>%
  mutate(se = sqrt(var_bl/n)) #creating a new column, use mutate, se, stadard error, term that we just gave for the new column

Eck %>% 
  group_by(site) %>% 
  summarise(var_bl = var(stipe_diameter), #then summarise for stipe diameter
            n = n()) %>%
  mutate(se = sqrt(var_bl/n))#creating a new column, use mutate, se, stadard error, term that we just gave for the new column

# Determine the min and maximum frond length and stipe length

Eck %>% 
  summarise(min_fl= min(frond_length), 
            max_fl=max(frond_length))

Eck %>% 
  summarise(min_sl= min(stipe_length),
            max_sl= max(stipe_length))

# Checking the overall summary of the dataset
summary(ecklonia)

# Section 3: 
# Make use of the SACTN_day1 data:

SAC <- read_csv("data/SACTN_day_1.csv") # Assigning the dataset a new name

# Here create a graph showing temperature variation between sites

SACTN_tidy <- SACTN %>% #Tidying our dataset
  separate(col = index, into = c("site", "src"), sep = "/ ") #Using a function separate to separate the index column into site and src

rm(SACTN) #First remove/delete the untidy dataset/original dataset.

SACTN_tidier <- SACTN_tidy %>% #Then arrange the columns so that set as follows: site,src,date and temp using the select function
  select(site, src, date, temp)

ggplot(data = SACTN_tidier,aes(x=date, y= temp)) + # A graph showing temperature variation at all sites
  geom_line(aes(colour = site, group = paste0(site))) +
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()

# Select all the temperatures recorded at the site Port Nolloth during August or September.
PortN <- SACTN_tidier%>% # Assign new name the filter site Port Nolloth
  filter(site == "Port Nolloth", month(date) == 08 | month(date) == 09)# Then filter by date extracting the 8th and 9th months(august and september)

SACTN_tidier %>%  #Selecting the monthly temperatures recorded in Port Nolloth during the year 1994.
  filter(site == "Port Nolloth",year(date) == 1994)

# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script
  
# Section 4:
# Make use of any two built in datasets:
  
carbon <- datasets::CO2
air <- datasets::airquality

# Make use of the summarise, select, group_by functions

carbon %>% 
  summarise(mean_conc = mean(conc),
            min_conc = min(conc))

air %>% 
  summarise(max_temp = mean(temp),
            min_temp = min(temp))

carbon %>% 
  select(Treatment.uptake)

air %>% 
  select(Solar.R,Wind)

#Visualizations not done in the R intro workshop

#Using geom_jitter then adding geom_line to connect he jitter points,then able to read the pattern of the data.

carbon %>% 
  ggplot(aes(x=Treatment, y=uptake)) +
  geom_jitter()+
  geom_line()

#Using the women1 data,geom_bin2d function creates a pattern based on the specified data and then adding geom_smooth makes it easy to read that patten.
air %>% 
  ggplot(aes(x= Solar.R, y= Wind))+
  geom_bin2d()+
  geom_smooth(method = "lm")

carbon %>% 
  ggplot(aes(x= Treatment, y= uptake))+
  geom_bin2d()+
  geom_smooth(method = "lm")

#using crossbar
air %>% 
  ggplot(aes(x= Solar.R, y= Wind))+
  geom_crossbar(ymin=115, ymax=155)
carbon %>% 
  ggplot(aes(x= Treatment, y= uptake))+
  geom_crossbar(ymin=115, ymax=155)


#Using geom_violin
air %>% 
  ggplot(aes(x= Solar.R, y= Wind)) +
  geom_violin()
carbon %>% 
  ggplot(aes(x= Treatment, y= uptake)) +
  geom_violin()







