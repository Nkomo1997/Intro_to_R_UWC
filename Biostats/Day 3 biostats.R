#Day 3 oof biostat
#linear regression

library(tidyverse)
library(fitdistrplus)
library(logspli)

datasets::faithful

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)
        
  ggplot(data = faithful, aes(x = eruptions, y= waiting)) +
  geom_point() +
    geom_smooth(method="lm") +
  labs( y = "waiting (min)", x = "eruptions (min)") + 
    ggtitle("A linear regression line showing the relationship between waiting and eruptions")

  slope <- round(eruption.lm$coef[2], 3)
  p.val = 0.001
  r2 <- round(summary(eruption.lm)$r.squared, 3)
  ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
    geom_point() +
    annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
    annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
    annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
    stat_smooth(method = "lm", colour = "salmon") +
    labs(title = "Old Faithful eruption data",
         subtitle = "Linear regression",
         x = "Waiting time (minutes)",
         y = "Eruption duration (minutes)")
  
  n <- 100
  set.seed(666)
  rand.df <- data.frame(x = seq(1:n),
                        y = rnorm(n = n, mean = 20, sd = 3))
  ggplot(data = rand.df, aes(x = x, y = y)) +
    geom_point(colour = "blue") +
    stat_smooth(method = "lm", colour = "purple", size = 0.75, fill = "turquoise", alpha = 0.3) +
    labs(title = "Random normal data",
         subtitle = "Linear regression",
         x = "X (independent variable)",
         y = "Y (dependent variable)")
  
  # Load data
  
  ecklonia <- read.csv("data/ecklonia.csv")
  # Perform correlation analysis on two specific variables
  # Note that we do not need the final two arguments in this function to be stated
  # as they are the defaut settings.
  # They are only shown here to illustrate that they exist.
  cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
           use = "everything", method = "pearson")
  
  ecklonia_sub <- ecklonia %/%
    select(-species, - site, - ID)
  
  
  ecklonia_pearson <- cor(ecklonia_sub)
  ecklonia_pearson
  
  ecklonia_norm <- ecklonia_sub %>% 
    gather(key = "variable") %>% 
    group_by(variable) %>% 
    summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
  ecklonia_norm
  
  # Calculate Pearson r beforehand for plotting
  r_print <- paste0("r = ", 
                    round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))
  
  ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
    geom_smooth(method = "lm", colour = "grey90", se = F) +
    geom_point(colour = "mediumorchid4") +
    geom_label(x = 300, y = 240, label = r_print) +
    labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
    theme_pubclean()
  
  corrplot(ecklonia_pearson, method = "circle")
  