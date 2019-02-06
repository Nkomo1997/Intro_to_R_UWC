#Thulisile Nkomo
#Excercise on plotting graphs


library(readr)
laminaria <- read_csv("data/laminaria.csv")
View(laminaria)

#Hypothesis

plot_Lam1 <- ggplot(laminaria, aes(x = total_length, y = stipe_mass, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe mass") + 
  ggtitle("A line graph showing the stipe mass of laminaria sp.
          with total length and sites")

plot_Lam2 <- ggplot(laminaria, aes(x = total_length, y = stipe_length, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe length") + 
  ggtitle("A line graph showing the stipe length of laminaria sp.
          with total length and sites")

plot_Lam3 <- ggplot(laminaria, aes(x = total_length, y = stipe_diameter, colour =site))+ 
  geom_point() + 
  geom_line(aes(group = region)) + labs(x ="Total length", y = "Stipe diameter") + 
  ggtitle("A line graph showing the stipe diameter of laminaria sp.
          with total length and sites")
