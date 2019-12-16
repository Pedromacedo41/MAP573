library(forecast)
library(ggplot2)

#set current directory to code source and remove NAs
p <- read.csv("./../../Data/Predict & Hyper/ari1.csv", header = F)
p = p[!is.na(p)]


#Autocorrelation...
p1 <- ggAcf(p) + labs(title="Autocorrelation of predicted values")+theme_bw()
p2 <- ggAcf(p, type = "partial") + labs(title="Partial autocorrelation of predicted values")+theme_bw()
grid.arrange(p1, p2, nrow = 2)



