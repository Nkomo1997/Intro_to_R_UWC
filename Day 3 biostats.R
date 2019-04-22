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

  # Script runs
  # NO comments on scrits
  # Seems like its just being copied and paste
  