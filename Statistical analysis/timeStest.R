
TS <- read.csv("../Data/TS.csv")
TST <- read.csv("../Data/TST.csv")


row.names(TST) <- TST$datetime
TST = subset(TST, select = -c(X, datetime))

row.names(TS) <- TS$datetime
TS = subset(TS, select = -c(X, datetime))


#I assume the temperature zones (1-11) are Z1-Z11  in the loadTS.


#Now we can plot the temperature vs load
#plot(TST, TS) #Can be done but zones will form different clusters
#Maybe better to take the average across all zones



#STANDARDIZE
Z <- t(as.matrix(TS))
TSstandardized <-  t(scale(Z, center = TRUE, scale = TRUE))
Z <- t(as.matrix(TST))
TSTstandardized <- t(scale(Z, center = TRUE, scale = TRUE))



TSmeans <- rowMeans(TS[1:29414, ])
TSmeans[1]
TSmeans_standardized <- rowMeans(TSstandardized[1:29414, ])

TSTmeans <- rowMeans(TST[1:29414, ])
TSTmeans[1]
TSTmeans_standardized <- rowMeans(TSTstandardized[1:29414, ])


data <- as.data.frame(cbind(TSTmeans, TSmeans))
data_standardized <- as.data.frame(cbind(TSTmeans_standardized, TSmeans_standardized))

plot(TSTmeans, TSmeans, 
     main = "Relationship between energy load and temperature", 
     xlab = "Temperature [F]", 
     ylab = "Load [xW]", 
     cex = 0.1,
     panel.first = grid(nx = NULL, ny = NULL, col = "red", lty = "dotted"))


plot(TSTmeans_standardized, TSmeans_standardized, 
     main = "Relationship between energy load and temperature (standardized)", 
     xlab = "Temperature", 
     ylab = "Load", 
     cex = 0.1,
     panel.first = grid(nx = NULL, ny = NULL, col = "red", lty = "dotted"))



#Try divide each value with the biggest value of each column and plotting again






acf(TSmeans, lag.max = NULL, plot = TRUE, na.action = na.pass)


library(zoo)

times <- as.POSIXct(row.names(data), format = "%Y-%m-%d %H:%M:")
timesNAomit <- na.omit(times) 
load.ts <- as.zoo(
  x         = data$TSmeans,
  order.by  = timesNAomit,
  frequency = 24
)

temperature.ts <- zoo(
  x         = data$TSTmeans,
  order.by  = timesNAomit,
  frequency = 24
)


plotData <- cbind(load.ts, temperature.ts)

library(ggplot2)
ggplot(plotData, aes(timesNAomit)) + 
  geom_line(aes(y = load.ts, colour = "Load")) + 
  geom_line(aes(y = temperature.ts, colour = "Temperature"))+
  labs(title= "Correlation of load and temperature",
     y="Value", x = "Year")





#_____________________ Create and work with TS____________________________#

library(forecast)
library(xts)


TS <- read.csv("../Data/TS.csv")
row.names(TS) <- TS$datetime
TS <- TS[,3:22]
colnames(TS) <-  c("Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8", "Z9", 
                      "Z10", "Z11", "Z12" ,"Z13", "Z14", "Z15", "Z16", "Z17", "Z18", "Z19", "Z20")


TS <- TS[1]
#TS
startDay <- as.POSIXct(row.names(TS)[1], format = "%Y-%m-%d %H:%M:")
load.ts <- ts(TS, start = as.numeric(startDay))

#ZOO
times <- as.POSIXct(row.names(TS), format = "%Y-%m-%d %H:%M:")
load.zoo <- as.zoo(
  x         = TS,
  order.by  = times,
  frequency = 24
)


#autoplot(load.zoo)
#autoplot(load.zoo, facets = FALSE)


gglagplot(load.ts, do.lines=FALSE)
ggAcf(load.ts, type = "correlation")
ggAcf(load.ts, type = "partial")


ggAcf(TS)
g <- ggAcf(load.ts, plot=FALSE)
plot(diff(load_xts))
