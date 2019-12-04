library(shiny)
library(dygraphs)
library(data.table)
TS= fread("./Data/Complete_TS.csv", sep=",")
load_table= data.frame(TS)

View(load_table$V2)
h <-as.numeric(load_table$V2)
View(h)
g <- (h-min(h))/(max(h)-min(h))
View(g)

reshape_X_3d <- function(X) {
  print(dim(X))
  dim(X) <- c(dim(X)[1],1, dim(X)[2])
  X
}
g <- reshape_X_3d(g)


build_matrix <- function(tseries, overall_timesteps) {
  
  t(sapply(1:(length(tseries) - overall_timesteps + 1), 
           function(x) tseries[x:(x + overall_timesteps - 1)]))
}

window <-100
wi <- g[30000:30099]


as <-c()
for(i in 1:300){
  test_matrix <- build_matrix(wi, window)
  data <- model  %>% predict(reshape_X_3d(test_matrix))
  as <- c(as,data[1,1])
  wi[1: (length(wi)-1)] <- wi[2:length(wi)]
  wi[length(wi)] =data[1,1]
}

View(as*(max(h)-min(h))+min((h)))
write.csv(as*(max(h)-min(h))+min((h)), file = "./Data/MyData2.csv")

View(reshape_X_3d(test_matrix))

reshape_X_3d(test_matrix)

library(keras)
model <- load_model_hdf5("./Data/Models/lstm_1h_window100.h5")

View(data)
View(data[,1]*(max(h)-min(h))+min((h)))
write.csv(data[,1]*(max(h)-min(h))+min((h)), file = "./Data/MyData.csv")

xd5= t(fread("./Data/MyData.csv", sep=","))
xd5= data.frame(t(xd5))
View(xd5$x)


library(forecast)
library(ggplot2)
library(gridExtra)

#set current directory to code source
p <- read.csv("./Data/PredictHyper/ari1.csv", header = F)
p = p[!is.na(p)]



p1 <- ggAcf(p) + labs(title="Autocorrelation of predicted values")+theme_bw()
p2 <- ggAcf(p, type = "partial") + labs(title="Partial autocorrelation of predicted values")+theme_bw()
grid.arrange(p1, p2, nrow = 2)




model <- load_model_hdf5("./Data/Models/lstm_1h_window100.h5")


TS <- read.csv("./Data/Complete_TS.csv")
TST <- read.csv("./Data/TST.csv")

View(TS)

library(keras)


model1 <- load_model_hdf5("./Data/Models/lstm_672_168.h5")
model2 <- load_model_hdf5("./Data/Models/lstm_100_1.h5")
model3 <- load_model_hdf5("./Data/Models/lstm_100_24.h5")
model4 <- load_model_hdf5("./Data/Models/lstm_1000_168.h5")



data <- as.data.frame(cbind(TST, TS))

times <- as.POSIXct(row.names(data), format = "%Y-%m-%d %H:%M:")
timesNAomit <- na.omit(times) #for NA values we omit 

load_zoo_zones <- zoo( # all zones
  x         = TS,
  order.by  = timesNAomit,
  frequency = 24
)


load_zoo <- zoo( #sum of the zones
  x         = data$TS_sum,
  order.by  = timesNAomit,
  frequency = 24
)

temperature_zoo <- zoo(
  x         = data$TST_mean,
  order.by  = timesNAomit,
  frequency = 24
)

data_zoo <- cbind(load_zoo, temperature_zoo)

load_ts <- ts(TS_sum, start = as.numeric(times[1]), frequency = 24) 













