library(forecast)
library(ggplot2)

#set current directory to code source
p <- read.csv("./../../Data/Predict & Hyper/bidirectional_longer.csv", header = F)
p = p[!is.na(p)]



ggAcf(p) + labs(title="Autocorrelations for ....")+theme_bw()
ggAcf(p, type = "partial") + labs(title="Autocorrelations for ....")+theme_bw()
