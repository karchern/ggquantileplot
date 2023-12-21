library(tidyverse)
library(grid)
library(ggplot2)
library(ggquantileplot)

pQuantileplot <- ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len)) +
    geom_quantileplot(aes(fill = as.factor(supp)), quantilesP = c(0.5, 0.7, 0.9, 1)) +
    scale_fill_manual(values = c("OJ" = "red", "VC" = "blue")) +
    scale_color_manual(values = c("OJ" = "red", "VC" = "blue")) +
    theme_classic()

ggsave(plot = pQuantileplot, filename = "/Users/karcher/ggquantileplot/test.png")
