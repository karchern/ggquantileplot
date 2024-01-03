library(tidyverse)
library(grid)
library(ggplot2)
library(ggquantileplot)




baseColors <- c("OJ" = "#c45e5e", "VC" = "#2635a6")
quantilesP <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

quantilePlot <- ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len)) +
    # First entry of quantilesP needs to be 0.5
    geom_quantileplot(aes(fill = supp), quantilesP = quantilesP) +
    # scale_fill_quantile is crucial here, otherwise color scale is all over the place!
    scale_fill_quantile(baseColors, quantilesP) +
    theme_classic() +
    guides(fill = guide_legend(ncol = 2)) +
    NULL

ggsave(plot = quantilePlot, filename = "/Users/karcher/ggquantileplot/test.png")
