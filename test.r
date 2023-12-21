library(tidyverse)
library(grid)
library(ggplot2)
library(ggquantileplot)

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len)) +
    geom_quantileplot(aes(fill = as.factor(supp))) +
    geom_point(aes(color = as.factor(supp), group = supp), position = position_jitterdodge()) +
    scale_fill_manual(values = c("OJ" = "red", "VC" = "blue")) +
    scale_color_manual(values = c('OJ' = "red", "VC" = "blue"))
