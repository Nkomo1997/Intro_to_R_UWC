#Thulisile Nkomo
#Excercise on plotting graphs


library(tidyverse)
iris <- datasets::iris

view(iris)
??iris

#Hypothesis (Plot_1). The sepal length is in direct proporrtion to the petal width
#for every iris species

Plot_1 <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Width, colour = Species)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Sepal.Length", y = "Petal.Width") + 
  ggtitle("A graph showing the relationship between petal (width) and 
                septal (length) of different species of iris") +  theme_bw()

#Hypothesis (Plot_2). The sepal width is in direct proporrtion to the petal length
#for every iris species

Plot_2 <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Length, colour = Species))+ 
  geom_point() + 
  geom_smooth(aes(method = "lm")) + 
  ggtitle("A graph showing the relationship between petal (length) and 
               septal (width) of different species of iris")

airquality <- datasets::airquality
??ability.cov

#Hypothesis (Plot_3). The changes in wind speed is dependant on temperature

Plot_3 <- ggplot(data = airquality, aes(x = Month, y = Wind, colour = Temp)) +
  geom_point(aes(size = Wind)) + geom_smooth(method = "lm") +
  labs(x = "Month", y = "Wind") + 
  ggtitle("A graph showing different changes in wind over 9 month 
          with the effect of Temperature") + theme_bw()

#Hypothesis (Plot_4). The changes in solar radiation is dependant on temperature

Plot_4 <- ggplot(data = airquality, aes(x = Month, y = Solar.R, colour = Temp)) +
  geom_point(aes(size = Solar.R)) + geom_smooth(method = "lm") +
  labs(x = "Month", y = "Solar.R") + 
  ggtitle("A graph showing different changes in Solar Radiation over 9 month 
          with the effect of Temperature") + theme_bw()

CO2 <- datasets::CO2

#Hypothesis (Plot_5).Plant material is more concetrated in chilled treatment 
#in comparion to nonchilled treatment

Plot_5 <- ggplot(data = CO2, aes(x = Plant, y = conc, colour = Treatment)) +
  geom_point(aes(size = conc)) + geom_smooth(method = "lm") +
  labs(x = "Plant", y = "conc") + 
                ggtitle("A graph showing different plants
                        at various concentrations in different treatments") + theme_bw()

#Hypothesis (Plot_5).Plant material is has a higher uptake rate in chilled treatment 
#in comparion to nonchilled treatment

Plot_6 <-  ggplot(data = CO2, aes(x = Plant, y = uptake, colour = Treatment)) +
  geom_point(aes(size = conc)) + geom_smooth(method = "lm") +
  labs(x = "Plant", y = "uptake") + 
  ggtitle("A graph showing uptake rates of 
different plants in different treatments") + theme_bw()



library(readr)
laminaria <- read_csv("data/laminaria.csv")
View(laminaria)

#Hypothesis. stipe mass is directly proportional to the total length

plot_Lam1 <- ggplot(laminaria, aes(x = total_length, y = stipe_mass, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe mass") + 
  ggtitle("A line graph showing the stipe mass of laminaria sp.
          with total length and sites")

#Hypothesis. stipe length is directly proportional to the total length

plot_Lam2 <- ggplot(laminaria, aes(x = total_length, y = stipe_length, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe length") + 
  ggtitle("A line graph showing the stipe length of laminaria sp.
          with total length and sites")

#Hypothesis. stipe diameter is independant to the total length pf the laminaria sp.

plot_Lam3 <- ggplot(laminaria, aes(x = total_length, y = stipe_diameter, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe diameter") + 
  ggtitle("A line graph showing the stipe diameter of laminaria sp.
          with total length and sites")

